package org.podval.calendar.service

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import org.digitaljudaica.metadata.{Language, LanguageSpec}
import org.http4s.{Charset, HttpRoutes, QueryParamDecoder, Response, StaticFile}
import org.http4s.implicits._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType
import org.http4s.server.blaze.BlazeServerBuilder
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

/*
  There is currently no need for the polished, public UI.
  This UI was done just for testing the underlying calculations, not for public consumption.
  That is why the following isn't being done:
  - put dayLinks into a table with no border and fixed column size;
  - deal with spacing and ltr/rtl;
  - clean up CSS styling;
  - complete internationalization;
  - make UI dynamic;

  I am also not doing an API service right now: there are no consumers for it in development,
  but it is possible at this point (12/2018) that one will appear soon, and then the API will
  be designed to accommodate the needs of the real consumer.
  Then, we'll:
  - communicate selective applicability of Purim/Shushan Purim readings;
  - add Nassi, Molad, Tehillim, Tachanun, Maariv after Shabbos...
 */
object CalendarService extends IOApp {

  private val blockingPool: ExecutionContextExecutorService =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  private val blocker: Blocker = Blocker.liftExecutorService(blockingPool)

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private val calendarService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case request @ GET -> Root / path if staticResourceExtensions.exists(path.endsWith) =>
      StaticFile.fromResource("/" + path, blocker, Some(request))
        .getOrElseF(NotFound())

    case GET -> Root
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    => renderHtml(Renderer.renderRoot(toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / kindStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderer(kindStr, maybeLocation, maybeLanguage).renderLanding)

    case GET -> Root / kindStr / yearStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderer(kindStr, maybeLocation, maybeLanguage).renderYear(yearStr))

    case GET -> Root / kindStr / yearStr / monthStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderer(kindStr, maybeLocation, maybeLanguage).renderMonth(yearStr, monthStr))

    case GET -> Root / kindStr / yearStr / monthStr / dayStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderer(kindStr, maybeLocation, maybeLanguage).renderDay(yearStr, monthStr, dayStr))
  }

  private implicit val languageQueryParamDecoder: QueryParamDecoder[Language] =
    QueryParamDecoder[String].map(Language.getForName)

  private object OptionalLanguageQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Language]("lang")

  private implicit val locationQueryParamDecoder: QueryParamDecoder[Location] =
    QueryParamDecoder[Boolean].map(if (_) Location.HolyLand else Location.Diaspora)

  private object OptionalInHolyLandQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Location]("inHolyLand")

  private def renderer(
    kindStr: String,
    maybeLocation: Option[Location],
    maybeLanguage: Option[Language]
  ): Renderer = Renderer.renderer(kindStr, toLocation(maybeLocation), toSpec(maybeLanguage))

  private def toSpec(language: Option[Language]): LanguageSpec = language.getOrElse(Language.English).toSpec

  private def toLocation(location: Option[Location]): Location = location.getOrElse(Location.Diaspora)

  def renderHtml(content: String): IO[Response[IO]] = Ok(content).map(
    _.withContentType(`Content-Type`(MediaType.`text`.`html`, Charset.`UTF-8`))
  )

  // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
  override def run(args: List[String]): IO[ExitCode] = BlazeServerBuilder[IO]
    .bindHttp(host = "0.0.0.0", port = getServicePort)
    .withHttpApp(calendarService.orNotFound)
    .serve
    .compile
    .drain
    .as(ExitCode.Success)

  private def getServicePort: Int =
    scala.util.Properties.envOrNone("SERVICE_PORT").map(_.toInt).getOrElse(8090)
}
