package org.opentorah.calendar.service

import io.netty.handler.codec.http.{HttpHeaderNames, HttpHeaderValues}
import org.opentorah.metadata.{Language, LanguageSpec}
import org.opentorah.util.{Logging, Zhttp}
import zhttp.http.*
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
object CalendarService extends zio.App:

  Logging.configureLogBack(useLogStash = false)

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private val routes = Http.collectM[Request] {
    case request @ Method.GET -> Root / path if staticResourceExtensions.exists(path.endsWith) =>
      Zhttp.staticResource("/" + path, Some(request))
        .catchAll(error => ZIO.succeed(Zhttp.notFound(path + "\n" + error.getMessage)))

    case request @ Method.GET -> Root =>
      renderHtml(Renderer.renderRoot(getLocation(request), getLanguage(request)))

    case request@ Method.GET -> Root / kindStr =>
      renderHtml(Renderer.renderLanding(kindStr, getLocation(request), getLanguage(request)))

    case request@ Method.GET -> Root / kindStr / yearStr =>
      renderHtml(Renderer.renderYear(kindStr, getLocation(request), getLanguage(request), yearStr))

    case request @ Method.GET -> Root / kindStr / yearStr / monthStr =>
      renderHtml(Renderer.renderMonth(kindStr, getLocation(request), getLanguage(request), yearStr, monthStr))

    case request@ Method.GET -> Root / kindStr / yearStr / monthStr / dayStr =>
      renderHtml(Renderer.renderDay(kindStr, getLocation(request), getLanguage(request), yearStr, monthStr, dayStr))
  }

  private def getLanguage(request: Request): LanguageSpec = Zhttp.queryParameter(request, "lang")
    .map(Language.getForName)
    .getOrElse(Language.English).toSpec

  private def getLocation(request: Request): Location = Zhttp.queryParameter(request, "inHolyLand")
    .map(value => value == "true")
    .map(if _ then Location.HolyLand else Location.Diaspora)
    .getOrElse(Location.Diaspora)

  def renderHtml(content: String): ResponseM[Any, Nothing] = ZIO.succeed(Response.http(
    headers = List(Header.custom(HttpHeaderNames.CONTENT_TYPE.toString, HttpHeaderValues.TEXT_HTML)), // TODO UTF-8?
    content = Zhttp.textData(content)
  ))

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, zio.ExitCode] = Zhttp.start(
    port = scala.util.Properties.envOrNone("PORT").map(_.toInt).getOrElse(8090),
    routes
  )
