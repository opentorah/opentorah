package org.opentorah.calendar.service

import io.netty.handler.codec.http.{HttpHeaderNames, HttpHeaderValues}
import org.opentorah.metadata.{Language, LanguageSpec}
import org.opentorah.util.{Logging, Zhttp}
import zhttp.http._
import zio.ZIO

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
  - add Nassi, Tachanun, Maariv after Shabbos...
 */
object CalendarService extends zio.App {

  Logging.configureLogBack(useLogStash = false)

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private val routes = Http.collectM[Request] {
    case request @ Method.GET -> Root / path if staticResourceExtensions.exists(path.endsWith) =>
      for {
        static <- Zhttp.staticResource("/" + path, Some(request))
      } yield static.getOrElse(Zhttp.notFound(path))

    case request @ Method.GET -> Root =>
      renderHtml(Renderer.renderRoot(getLocation(request), getLanguage(request)))

    case request@ Method.GET -> Root / kindStr =>
      renderHtml(renderer(kindStr, request).renderLanding)

    case request@ Method.GET -> Root / kindStr / yearStr =>
      renderHtml(renderer(kindStr, request).renderYear(yearStr))

    case request @ Method.GET -> Root / kindStr / yearStr / monthStr =>
      renderHtml(renderer(kindStr, request).renderMonth(yearStr, monthStr))

    case request@ Method.GET -> Root / kindStr / yearStr / monthStr / dayStr =>
      renderHtml(renderer(kindStr, request).renderDay(yearStr, monthStr, dayStr))
  }

  private def renderer(kindStr: String, request: Request): Renderer =
    Renderer.renderer(kindStr, getLocation(request), getLanguage(request))

  private def getLanguage(request: Request): LanguageSpec = Zhttp.queryParameter(request, "lang")
    .map(Language.getForName)
    .getOrElse(Language.English).toSpec

  private def getLocation(request: Request): Location = Zhttp.queryParameter(request, "inHolyLand")
    .map(value => value == "true")
    .map(if (_) Location.HolyLand else Location.Diaspora)
    .getOrElse(Location.Diaspora)

  def renderHtml(content: String): ResponseM[Any, Nothing] = ZIO.succeed(Response.http(
    headers = List(Header(HttpHeaderNames.CONTENT_TYPE, HttpHeaderValues.TEXT_HTML)), // TODO UTF-8?
    content = Zhttp.textData(content)
  ))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = Zhttp.start(
    port = scala.util.Properties.envOrNone("PORT").map(_.toInt).getOrElse(8090),
    routes
  )
}
