package org.opentorah.collector

import java.io.File
import java.net.URL
import java.util.concurrent.Executors
import org.http4s.{HttpRoutes, Request, Response, StaticFile, Status, Uri}
import org.http4s.dsl.Http4sDsl
import cats.effect.Blocker
import org.slf4j.{Logger, LoggerFactory}
import zio.duration.Duration
import zio.interop.catz._

final class Api(otherHost: String) {

  private val logger: Logger = LoggerFactory.getLogger(classOf[Api])

  private val dsl: Http4sDsl[ServiceTask] = Http4sDsl[ServiceTask]
  import dsl._

  def routes: HttpRoutes[ServiceTask] = HttpRoutes.of[ServiceTask] {
    case GET -> Root / "hello" =>
      Ok("hello!")

    case request@GET -> _ =>
      fromUrl(reTarget(request.uri), request)
  }

  private val blocker: Blocker = Blocker.liftExecutorService(Executors.newFixedThreadPool(2))

  private def fromUrl(url: URL, request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = {
    def notFound: ServiceTask[Response[ServiceTask]] = {
      NotFound(s"Not found: $url")
    }

    StaticFile
      .fromURL/*[ServiceTask]*/(url, blocker, Some(request))
      .getOrElseF(notFound)
      .timed.mapEffect { case (duration: Duration, response: Response[ServiceTask]) =>
        val durationStr = formatDuration(duration)
        if (response.status.isInstanceOf[Status.NotFound.type])
          Log.warning(logger, request, s"NOT $durationStr $url")
        else
          Log.info(logger, request, s"GOT $durationStr $url")
        response
      }
  }

  private def reTarget(uri: Uri): URL = {
    val result = uri.copy(
      scheme = Some(Uri.Scheme.http),
      authority = Some(Uri.Authority(host = Uri.RegName(otherHost))),
      path = if (new File(uri.path).isDirectory) uri.path + "index.html" else uri.path
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
