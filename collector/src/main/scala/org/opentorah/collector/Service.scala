package org.opentorah.collector

import cats.data.OptionT
import cats.effect.{Blocker, ExitCode, Sync}
import net.logstash.logback.argument.StructuredArgument
import org.opentorah.util.Files
//import cats.implicits._
//import fs2.io
import fs2.Stream
import net.logstash.logback.argument.StructuredArguments
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.{`Content-Length`, `Content-Type`}
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.util.CaseInsensitiveString
import org.http4s._
import org.slf4j.{Logger, LoggerFactory}
import zio.duration.Duration
import zio.interop.catz._
import zio.{App, RIO, URIO, ZEnv, ZIO}
import java.net.URL
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object Service extends App {
  LoggerFactory.getILoggerFactory.asInstanceOf[ch.qos.logback.classic.LoggerContext]
    .getLogger(Logger.ROOT_LOGGER_NAME).setLevel(ch.qos.logback.classic.Level.INFO)

  type ServiceEnvironment = ZEnv

  type ServiceTask[+A] = RIO[ServiceEnvironment, A]

  // TODO HTTP GET from http://metadata.google.internal/computeMetadata/v1/project/project-id
  // with `Metadata-Flavor: Google` header;
  // or do I even need it for logging?
  private val projectId: String = "alter-rebbe-2"

  val bucketName: String = "store.alter-rebbe.org"

  val dsl: Http4sDsl[ServiceTask] = Http4sDsl[ServiceTask]
  import dsl._

  override def run(args: List[String]): URIO[ServiceEnvironment, zio.ExitCode] = {
    def getParameter(name: String, defaultValue: String): String = Option(System.getenv(name)).fold {
      info(s"No value for '$name' in the environment; using default: '$defaultValue'")
      defaultValue
    }{ value =>
      info(s"Value    for '$name' in the environment: $value")
      value
    }

    val siteUrl: String = if (args.nonEmpty) {
      val result = args.head
      info(s"siteUri argument supplied: $result")
      result
    } else getParameter("STORE", s"http://$bucketName/")

    val port: Int = getParameter("PORT", "4000").toInt

    val executionContext: ExecutionContextExecutor = ExecutionContext.global

    // This is supposed to be set when running in Cloud Run
    val serviceName: Option[String] = Option(System.getenv("K_SERVICE"))
    warning(s"serviceName=$serviceName")

    ZIO.runtime[ServiceEnvironment].flatMap { implicit rts =>
      BlazeServerBuilder[ServiceTask](executionContext)
        // To be accessible when running in a docker container the server
        // must bind to all IPs, not just 127.0.0.1:
        .bindHttp(port, "0.0.0.0")
        .withWebSockets(false)
        .withHttpApp(routes(siteUrl).orNotFound)
        .serve
        .compile[ServiceTask, ServiceTask, ExitCode]
        .drain
    }
      .mapError(err => zio.console.putStrLn(s"Execution failed with: $err"))
      .exitCode
  }

  val blocker: Blocker = Blocker.liftExecutorService(Executors.newFixedThreadPool(2))

  private def routes(siteUrl: String): HttpRoutes[ServiceTask] = {
    val siteUri: Uri = Uri.unsafeFromString(siteUrl)

    var site: Option[Site] = None
    getSite(None)

    def getSite(request: Option[Request[ServiceTask]]): Site = site.getOrElse {
      info(request, "INI")
      val result = Site.read(toUrl(siteUri))
      site = Some(result)
      result
    }

    def fromSite(path: Seq[String], request: Request[ServiceTask]): Option[Response[ServiceTask]] = {
      val site: Site = getSite(Some(request))
      val storePath: Option[Store.Path] = site.resolve(path)

      val content: Option[(String, Boolean)] = try storePath.map(site.content) catch {
        case _: NotImplementedError => None
      }

      val result: Option[Response[ServiceTask]] = content.map { case (content: String, isTei: Boolean) =>
        val bytes: Array[Byte] = content.getBytes

        Response(
          headers = Headers(List(
            `Content-Type`(if (isTei) MediaType.application.`tei+xml` else MediaType.text.html, Charset.`UTF-8`),
            `Content-Length`.unsafeFromLong(bytes.length)
            // TODO more headers!
          )),
          body = Stream.emits(bytes)
        )
      }

      info(request, result.fold {
        storePath.fold(s"--- ${Files.mkUrl(path)}")(storePath => s"??? $storePath")
      }{ _ =>
        s"YES ${storePath.get}"
      })

      result
    }

    def get(request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = {
      // TODO if I add a parameter list (implicit F: Sync[ServiceTask]) and use F.pure and F.suspend, I get
      //   ambiguous implicit values:
      //   both method taskConcurrentInstance in trait CatsEffectInstances1 of type
      //     [R]cats.effect.Concurrent[[β$18$]zio.ZIO[R,Throwable,β$18$]]
      //   and value F of type cats.effect.Sync[org.opentorah.collector.Service.ServiceTask]
      //   match expected type cats.effect.Sync[org.opentorah.collector.Service.ServiceTask]
      val F: Sync[ServiceTask] = taskConcurrentInstance

      val path: Seq[String] = Files.splitAndDecodeUrl(request.uri.path)
      val urlStr: String = Files.mkUrl(path)

      val host: Option[String] = request.headers.get(CaseInsensitiveString("Host")).map(_.value)
      val staticOnly: Boolean = host.exists(_.startsWith("www."))
      if (!staticOnly) info(request, s"DYN host=${host.get}")

      OptionT(
        if (staticOnly) F.pure(None)
        else F.suspend(F.pure(fromSite(path, request)))
      )
      .orElse(StaticFile.fromURL[ServiceTask](
        toUrl(siteUri.resolve(relativize(addIndex(request.uri)))),
        Service.blocker,
        Some(request)
      ))
      .getOrElseF(NotFound(s"Not found: $urlStr"))
      .timed.mapEffect { case (duration: Duration, response: Response[ServiceTask]) =>
        val durationStr: String = Service.formatDuration(duration)

        if (response.status.isInstanceOf[NotFound.type])
          Service.warning(request, s"NOT $durationStr $urlStr")
        else
          Service.info   (request, s"GOT $durationStr $urlStr")

        response
      }
    }

    HttpRoutes.of[ServiceTask] {
      case request@GET -> Root / "reset-cached-site" =>
        site = None
        getSite(Some(request))
        info(request, "RST")
        Ok("Site reset!")

      case request@GET -> _ => get(request)
    }
  }

  private def addIndex(uri: Uri): Uri =
    if (uri.path.endsWith("/")) uri.copy(path = uri.path + "index.html") else uri

  private def relativize(uri: Uri): Uri =
    if (uri.path.startsWith("/")) uri.copy(path = uri.path.substring(1)) else uri

  private def toUrl(uri: Uri): URL = new URL(uri.toString)

  private def formatDuration(duration: Duration): String = {
    val millis: Long = duration.toMillis
    if (millis < 1000) s"$millis ms" else {
      val seconds: Float = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"
    }
  }

  private def info   (request: Option[Request[ServiceTask]], message: String): Unit = log(request, message, "INFO"   )
  private def info   (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "INFO"   )
  private def info   (                               message: String): Unit = log(None         , message, "INFO"   )
//  private def notice (request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "NOTICE" )
//  private def notice (                               message: String): Unit = log(None         , message, "NOTICE" )
  private def warning(request: Request[ServiceTask], message: String): Unit = log(Some(request), message, "WARNING")
  private def warning(                               message: String): Unit = log(None         , message, "WARNING")

  private val logger: Logger = LoggerFactory.getLogger("org.opentorah.collector.service.Service")

  private def log(request: Option[Request[ServiceTask]], message: String, severity: String): Unit = {
    val trace: Option[String] = request
      .flatMap(_
        .headers.get(CaseInsensitiveString("X-Cloud-Trace-Context"))
        .map(_.value.split("/")(0))
      )

    val arguments: Seq[StructuredArgument] =
      Seq(StructuredArguments.keyValue("severity", severity)) ++
      trace.toSeq.map(trace => StructuredArguments.keyValue("logging.googleapis.com/trace", s"projects/$projectId/traces/$trace"))

    logger.info(message, arguments: _*)
  }
}
