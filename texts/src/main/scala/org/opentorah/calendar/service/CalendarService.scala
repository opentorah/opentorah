package org.opentorah.calendar.service

import org.opentorah.metadata.Language
import org.opentorah.service.ServiceApp
import ServiceApp.given
import org.slf4j.{Logger, LoggerFactory}
import zhttp.http.{!!, /, Body, Headers, HeaderValues, Http, Method, Request, Response, *} // TODO remove '*'
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
object CalendarService extends ServiceApp:
  override protected def projectId: String = "???"
  override protected def portDefault: Int = 8090
  override protected def run(args: zio.Chunk[String]): ZIO[Any, Throwable, Any] = serve(routes)

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private def routes = Http.collectZIO[Request] {
    case request @ Method.GET -> !! / path if staticResourceExtensions.exists(path.endsWith) =>
      ServiceApp.orNotFound(path, ServiceApp.staticResource("/" + path, Some(request)))

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

  private def getLanguage(request: Request): Language.Spec = ServiceApp.queryParameter(request, "lang")
    .map(Language.getForName)
    .getOrElse(Language.English).toSpec

  private def getLocation(request: Request): Renderer.Location =
    val parameter = ServiceApp.queryParameter(request, "inHolyLand")
    val holyLand: Boolean = parameter.forall(_ == "true")
    if holyLand then Renderer.Location.HolyLand else Renderer.Location.Diaspora

  def renderHtml(content: String): zio.UIO[Response] = ZIO.succeed(Response(
    headers = Headers.contentType(HeaderValues.textHtml), // TODO UTF-8?
    body = Body.fromString(content)
  ))
