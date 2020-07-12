package org.opentorah.collector

import java.net.URL
import java.util.concurrent.Executors
import org.http4s.{HttpRoutes, Request, Response, StaticFile, Uri}
import org.http4s.dsl.Http4sDsl
import cats.effect.Blocker
//import cats.implicits._
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

  private def fromUrl(url: URL, request: Request[ServiceTask]): ServiceTask[Response[ServiceTask]] = {
    def notFound: ServiceTask[Response[ServiceTask]] = {
      val message: String = s"Not found: $url"
      Log.warning(logger, request, message)
      NotFound(message)
    }

//    val x = SyncServiceTask[_]].suspend(MyStaticFile
//      .fromURL(url, blocker, Some(request)))

    MyStaticFile
      .fromURL/*[ServiceTask]*/(url, blocker, Some(request))
      .getOrElseF(notFound)
      .timed.mapEffect { case (duration: Duration, result: Response[ServiceTask]) =>
        Log.info(logger, request, s"GOT ${formatDuration(duration)} $url")
        result
      }


//    StaticFile.fromURL[F](url, blocker)
//      .semiflatMap { res =>
//        res.body.chunks // chunks for better efficiency
//          .pull // start pulling to replay the original body stream
//          .fold(Stream.empty.covaryAll[F, Byte]) { _ ++ Stream.chunk(_) }
//          .flatMap { restream =>
//            Pull.output1(res.withBodyStream(restream))
//          }
//          .recoverWith { case ex: FileNotFoundException =>
//            Pull.output1(Response[F](status = Status.NotFound).withEntity("not found: " + ex.getMessage))
//          }
//          .stream // Stream[F, Response[F]]
//          .compile
//          .lastOrError // there should be a single `Response` entity after all
//      }
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
