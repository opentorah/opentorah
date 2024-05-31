package org.opentorah.calendar.service

import org.opentorah.calendar.Calendar
import org.opentorah.gcp.{GCP, GCPLogger}
import org.opentorah.metadata.Language
import org.opentorah.util.Logging
import org.slf4j.LoggerFactory
import zio.http.codec.PathCodec
import zio.http.codec.PathCodec.empty
import zio.http.{Body, Handler, handler, Header, Headers, HttpApp, MediaType, Method, Request, Response, Routes, Server, Status, string}
import zio.{ZIO, ZLayer}

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
  private val serviceName: Option[String] = GCP.getServiceName

  Logging.configureLogBack(useLogStash = serviceName.isDefined)

  private lazy val logger: GCPLogger = GCPLogger("???", LoggerFactory.getLogger(this.getClass))

  private def getParameter(name: String, defaultValue: String): String =
    def result(value: String, message: String): String =
      logger.info(message)
      value

    scala.util.Properties.envOrNone(name).map((value: String) =>
      result(value, s"Value    for '$name' in the environment; using it     : '$value'")
    ).getOrElse(
      result(defaultValue, s"No value for '$name' in the environment; using default: '$defaultValue'")
    )

  final override def run: ZIO[Environment, Any, Any] = //  ZIO[Any, Throwable, Nothing]
    val port: Int = getParameter("PORT", 8080.toString).toInt
    logger.warning(s"serviceName=$serviceName; port=$port") // TODO more information

    // Note: To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1.
    val config: Server.Config = Server.Config.default.port(port)

    val app: HttpApp[Any] = routes.sandbox.toHttpApp

    Server.serve(app).provide(
      Server.live,
      ZLayer.succeed(config)
    )

  private def routes: Routes[Any, Throwable] = Routes(
    Method.GET / "favicon.ico" -> Handler.fromResource("favicon.ico"),
    Method.GET / "style.css"   -> Handler.fromResource("style.css"),

    Method.GET / empty ->
      handler { (request: Request) =>
        renderHtml(Renderer.renderRoot(Location.fromRequest(request), Lang.fromRequest(request)))
      },

    Method.GET / string("calendarStr") ->
      handler { (calendarStr: String, request: Request) =>
        renderHtml(calendarStr, request, _.renderLanding)
      },

    Method.GET / string("calendarStr") / string("year") ->
      handler { (calendarStr: String, year: String, request: Request) =>
        renderHtml(calendarStr, request, _.renderYear(year))
      },

    Method.GET / string("calendarStr") / string("year") / string("month") ->
      handler { (calendarStr: String, year: String, month: String, request: Request) =>
        renderHtml(calendarStr, request, _.renderMonth(year, month))
      },

    Method.GET / string("calendarStr") / string("year") / string("month") / string("day") ->
      handler { (calendarStr: String, year: String, month: String, day: String, request: Request) =>
        renderHtml(calendarStr, request, _.renderDay(year, month, day))
      }
  )

  private def renderHtml(calendarStr: String, request: Request, render: Renderer => String): zio.UIO[Response] =
    val calendar: Calendar = Renderer
      .calendars
      .find(_.name == calendarStr)
      .getOrElse(throw IllegalArgumentException(s"Unrecognized calendar $calendarStr"))
    val location: Location = Location.fromRequest(request)
    val lang: Language.Spec = Lang.fromRequest(request)
    renderHtml(render(Renderer(calendar, location, lang)))

  private def renderHtml(content: String): zio.UIO[Response] = ZIO.succeed(Response(
    headers = Headers(Header.ContentType(MediaType.text.html)), // TODO UTF-8?
    body = Body.fromString(content)
  ))
