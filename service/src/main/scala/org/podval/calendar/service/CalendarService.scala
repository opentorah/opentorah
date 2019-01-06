package org.podval.calendar.service

import cats.effect.IO
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import org.http4s.{Charset, HttpService, QueryParamDecoder, Request, Response, StaticFile}
import org.http4s.dsl.io._ // missing something for Ok.map(): {GET, Root, OptionalQueryParamDecoderMatcher, Ok, NotFound}
import org.http4s.headers.`Content-Type`
import org.http4s.MediaType
import org.http4s.server.blaze.BlazeBuilder
import org.podval.judaica.metadata.{Language, LanguageSpec}

import scala.concurrent.ExecutionContext.Implicits.global

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
object CalendarService extends StreamApp[IO] {

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")
  private def isStaticResource(path: String): Boolean = staticResourceExtensions.exists(path.endsWith)
  private def serveStaticResource(path: String, request: Request[IO]): IO[Response[IO]] =
    StaticFile.fromResource("/" + path, Some(request)).getOrElseF(NotFound())

  private val calendarService: HttpService[IO] = HttpService[IO] {
    case request @ GET -> Root / path if isStaticResource(path) => serveStaticResource(path, request)

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
    _.withContentType(`Content-Type`(MediaType.`text/html`, Charset.`UTF-8`))
  )

  // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
  private val builder: BlazeBuilder[IO] = BlazeBuilder[IO]
    .bindHttp(host = "0.0.0.0", port = getServicePort)
    .mountService(calendarService, prefix = "/")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    builder.serve

  private def getServicePort: Int =
    scala.util.Properties.envOrNone("SERVICE_PORT").map(_.toInt).getOrElse(8090)
}
