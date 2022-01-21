package org.opentorah.calendar.service

import io.netty.handler.codec.http.{HttpHeaderNames, HttpHeaderValues}
import org.opentorah.metadata.Language
import org.opentorah.util.{Logging, Zhttp}
import Zhttp.given
import zhttp.http.*
import zio.{UIO, ZIO}

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
object CalendarService extends zio.ZIOAppDefault:

  Logging.configureLogBack(useLogStash = false)

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private val routes = Http.collectZIO[Request] {
    case request @ Method.GET -> !! / path if staticResourceExtensions.exists(path.endsWith) =>
      Zhttp.staticResource("/" + path, Some(request))
        .catchAll(error => ZIO.succeed(Zhttp.notFound(path + "\n" + error.getMessage)))

    case request @ Method.GET -> !! =>
      renderHtml(Renderer.renderRoot(getLocation(request), getLanguage(request)))

    case request@ Method.GET -> !! / kindStr =>
      renderHtml(Renderer.renderLanding(kindStr, getLocation(request), getLanguage(request)))

    case request@ Method.GET -> !! / kindStr / yearStr =>
      renderHtml(Renderer.renderYear(kindStr, getLocation(request), getLanguage(request), yearStr))

    case request @ Method.GET -> !! / kindStr / yearStr / monthStr =>
      renderHtml(Renderer.renderMonth(kindStr, getLocation(request), getLanguage(request), yearStr, monthStr))

    case request@ Method.GET -> !! / kindStr / yearStr / monthStr / dayStr =>
      renderHtml(Renderer.renderDay(kindStr, getLocation(request), getLanguage(request), yearStr, monthStr, dayStr))
  }

  private def getLanguage(request: Request): Language.Spec = Zhttp.queryParameter(request, "lang")
    .map(Language.getForName)
    .getOrElse(Language.English).toSpec

  private def getLocation(request: Request): Renderer.Location =
    Renderer.getLocation(Zhttp.queryParameter(request, "inHolyLand"))

  def renderHtml(content: String): UIO[Response] = ZIO.succeed(Response(
    headers = Headers.contentType(HeaderValues.textHtml), // TODO UTF-8?
    data = Zhttp.textData(content)
  ))

  override def run: zio.URIO[zio.ZEnv, zio.ExitCode] = Zhttp.start(
    port = scala.util.Properties.envOrNone("PORT").map(_.toInt).getOrElse(8090),
    routes
  )
