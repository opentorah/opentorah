package org.opentorah.collector

import cats.effect.{Blocker, ExitCode}
import net.logstash.logback.argument.StructuredArguments
import java.net.URL
import java.util.concurrent.Executors
import org.http4s.{HttpRoutes, Request, Response, StaticFile, Status, Uri}
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.util.CaseInsensitiveString
import org.slf4j.{Logger, LoggerFactory}
import scala.concurrent.ExecutionContext
import zio.{App, RIO, URIO, ZEnv, ZIO}
import zio.duration.Duration
import zio.interop.catz._

object Service extends App {
  type ServiceEnvironment = ZEnv

  type ServiceTask[+A] = RIO[ServiceEnvironment, A]

  private val projectId: String = "alter-rebbe-2"

  val dsl: Http4sDsl[ServiceTask] = Http4sDsl[ServiceTask]
  import dsl._

  override def run(args: List[String]): URIO[Service.ServiceEnvironment, zio.ExitCode] = {
    val storeUri: Uri = Uri.unsafeFromString(getParameter("STORE", "https://store.alter-rebbe.org"))
    run(ServiceRoutes.routes(storeUri))
  }

  def run(routes: HttpRoutes[ServiceTask]): URIO[ServiceEnvironment, zio.ExitCode] =
    ZIO.runtime[ServiceEnvironment].flatMap { implicit rts =>
      BlazeServerBuilder[ServiceTask](executionContext = ExecutionContext.global)
        .bindHttp(
          // To be accessible when running in a docker container the server
          // must bind to all IPs, not just 127.0.0.1:
          host = "0.0.0.0",
          port = getParameter("PORT", "4000").toInt
        )
        .withWebSockets(false)
        .withHttpApp(routes.orNotFound)
        .serve
        .compile[ServiceTask, ServiceTask, ExitCode]
        .drain
    }
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err"))
      .exitCode

  private def getParameter(name: String, defaultValue: String): String = scala.util.Properties.envOrNone(name).fold {
    notice(s"No value for '$name' in the environment; using default: '$defaultValue'")
    defaultValue
  }{ value =>
    notice(s"Value for '$name' from the environment: $value")
    value
  }

  val blocker: Blocker = Blocker.liftExecutorService(Executors.newFixedThreadPool(2))

  def fromUri(uri: Uri, request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] =
    X.fromUri(uri, request)

  def log(
    url: URL,
    request: Request[ServiceTask],
    duration: Duration,
    status: Status
  ): Unit = {
    val durationStr: String = formatDuration(duration)
    // TODO suppress URL encoding.
    val urlStr: String = url.toString
    if (status.isInstanceOf[NotFound.type])
      warning(request, s"NOT $durationStr $urlStr")
    else
      info(request, s"GOT $durationStr $urlStr")
  }

  private def formatDuration(duration: Duration): String = {
    val millis: Long = duration.toMillis
    if (millis < 1000) s"$millis ms" else {
      val seconds = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"
    }
  }

  private def info   (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "INFO"   )
  private def notice (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "NOTICE" )
  private def notice (                               message: String): Unit = log(None         , message, "NOTICE" )
  private def warning(request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "WARNING")

  private val logger: Logger = LoggerFactory.getLogger("org.opentorah.collector.service.ServiceConfiguration")

  private def log(request: Option[Request[ServiceTask]], message: String, severity: String): Unit = {
    val trace: String = request
      .flatMap { _
        .headers.get(CaseInsensitiveString("X-Cloud-Trace-Context"))
        .map(_.value.split("/")(0))
      }
      .getOrElse("no-trace")

    logger.info(
      message,
      StructuredArguments.keyValue("severity", severity),
      StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace"),
      null
    )
  }
}

// TODO figure out why doesn't this compile when I unfold it into Service - and do it!
private object X {
  import Service.ServiceTask
  import Service.dsl._

  def fromUri(uri: Uri, request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = {
    val url: URL = new URL(uri.toString)
    StaticFile
      .fromURL[ServiceTask](url, Service.blocker, Some(request))
      .getOrElseF(NotFound(s"Not found: $url"))
      .timed.mapEffect {
      case (duration: Duration, response: Response[ServiceTask]) =>
        Service.log(url, request, duration, response.status)
        response
    }
  }
}
