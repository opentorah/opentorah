package org.opentorah.collector

import java.net.URL
import java.util.concurrent.Executors
import org.http4s.{HttpRoutes, Request, Response, StaticFile, Uri}
import org.http4s.dsl.Http4sDsl
import cats.effect.Blocker
import org.slf4j.{Logger, LoggerFactory}
import zio.duration.Duration
//import zio._
import zio.interop.catz._

final class Api(otherHost: String) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Api])

  private val dsl: Http4sDsl[ServiceTask] = Http4sDsl[ServiceTask]
  import dsl._

  def routes: HttpRoutes[ServiceTask] = HttpRoutes.of[ServiceTask] {
    case GET -> Root / "hello" => Ok("hello!")
    case request@GET -> _ => fromUrl(reTarget(request.uri), request)
  }

  private val blocker: Blocker = Blocker.liftExecutorService(Executors.newFixedThreadPool(2))

  private def fromUrl(url: URL, request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = StaticFile
    .fromURL(url, blocker, Some(request))
    .getOrElseF {
      val message: String = s"Not found: $url"
      Log.warning(logger, request, message)
      NotFound(message)
    }
    .timed.mapEffect { case (duration: Duration, result: Response[ServiceTask]) =>
    result.body
      Log.info(logger, request, s"GOT ${formatDuration(duration)} $url")
      result
    }

  private def reTarget(uri: Uri): URL = {
    val result = uri.copy(
      scheme = Some(Uri.Scheme.http),
      authority = Some(Uri.Authority(host = Uri.RegName(otherHost)))
    )
    new URL(result.toString)
  }

  private def formatDuration(duration: Duration): String = {
    val millis: Long = duration.toMillis
    if (millis < 1000) s"$millis ms" else {
      val seconds = Math.round(millis.toFloat/100).toFloat/10
      s"$seconds s"
    }
  }
}
