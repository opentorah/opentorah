package org.opentorah.calendar.service

import org.opentorah.calendar.Calendar
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.Gregorian
import org.opentorah.metadata.Language
import org.opentorah.service.{ServiceApp, Static}
import ServiceApp.given
import zio.http.codec.{HttpCodec, HttpCodecType, PathCodec}
import zio.http.endpoint.{Endpoint, EndpointMiddleware, Routes}
import zio.http.{->, /, Body, Header, Headers, Http, MediaType, Method, Request, Response, Root}
import zio.{ZIO, ZNothing}

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

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private def routes = Http.collectZIO[Request] {
    case request @ Method.GET -> Root / path if staticResourceExtensions.exists(path.endsWith) =>
      ServiceApp.orNotFound(path, Static.resource("/" + path, Some(request)))

    case request @ Method.GET -> Root =>
      renderHtml(Renderer.renderRoot(Location.get(request), Lang.get(request)))

    case request@ Method.GET -> Root / calendarStr =>
      renderHtml(Renderer(CalendarService.getCalendar(calendarStr), Location.get(request), Lang.get(request)).renderLanding)

    case request@ Method.GET -> Root / calendarStr / year =>
      val calendar: Calendar = CalendarService.getCalendar(calendarStr)
      renderHtml(Renderer(calendar, Location.get(request), Lang.get(request)).renderYear(year))

    case request @ Method.GET -> Root / calendarStr / year / month =>
      val calendar: Calendar = CalendarService.getCalendar(calendarStr)
      renderHtml(Renderer(calendar, Location.get(request), Lang.get(request)).renderMonth(year, month))

    case request@ Method.GET -> Root / calendarStr / year / month / day =>
      val calendar: Calendar = CalendarService.getCalendar(calendarStr)
      renderHtml(Renderer(calendar, Location.get(request), Lang.get(request)).renderDay(year, month, day))
  }


  private val rootEndpoint: Endpoint[(Location, Language.Spec), ZNothing, String, EndpointMiddleware.None] = Endpoint
    .get(HttpCodec.empty)
    .query(Location.codec)
    .query(Lang.codec)
    .out[String]

  private val calendarEndpoint: Endpoint[(Calendar, Location, Language.Spec), ZNothing, String, EndpointMiddleware.None] = Endpoint
    .get(CalendarService.calendarCodec)
    .query(Location.codec)
    .query(Lang.codec)
    .out[String]

  private val landingEndpoint: Endpoint[(Calendar, Location, Language.Spec), ZNothing, String, EndpointMiddleware.None] =
    calendarEndpoint

  private val yearCodec : PathCodec[String] = HttpCodec.string("year" )
  private val monthCodec: PathCodec[String] = HttpCodec.string("month")
  private val dayCodec  : PathCodec[String] = HttpCodec.string("day"  )

  private val yearEndpoint: Endpoint[(Calendar, Location, Language.Spec, String), ZNothing, String, EndpointMiddleware.None] =
    calendarEndpoint.path(yearCodec)

  private val monthEndpoint: Endpoint[(Calendar, Location, Language.Spec, String, String), ZNothing, String, EndpointMiddleware.None] =
    calendarEndpoint.path(yearCodec / monthCodec)

  private val dayEndpoint: Endpoint[(Calendar, Location, Language.Spec, String, String, String), ZNothing, String, EndpointMiddleware.None] =
    calendarEndpoint.path(yearCodec / monthCodec / dayCodec)

  private val rootRoute: Routes[Any, ZNothing, EndpointMiddleware.None] = rootEndpoint.implement {
    case (location: Location, language: Language.Spec) =>
      ZIO.succeed(Renderer.renderRoot(location, language))
  }

  private val landingRoute: Routes[Any, ZNothing, EndpointMiddleware.None] = landingEndpoint.implement {
    case (calendar: Calendar, location: Location, language: Language.Spec) =>
      ZIO.succeed(Renderer(calendar, location, language).renderLanding)
  }

  private val yearRoute: Routes[Any, ZNothing, EndpointMiddleware.None] = yearEndpoint.implement {
    case (calendar: Calendar, location: Location, language: Language.Spec, year: String) =>
      ZIO.succeed(Renderer(calendar, location, language).renderYear(year))
  }

  private val monthRoute: Routes[Any, ZNothing, EndpointMiddleware.None] = monthEndpoint.implement {
    case (calendar: Calendar, location: Location, language: Language.Spec, year: String, month: String) =>
      ZIO.succeed(Renderer(calendar, location, language).renderMonth(year, month))
  }

  private val dayRoute: Routes[Any, ZNothing,EndpointMiddleware. None] = dayEndpoint.implement {
    case (calendar: Calendar, location: Location, language: Language.Spec, year: String, month: String, day: String) =>
      ZIO.succeed(Renderer(calendar, location, language).renderDay(year, month, day))
  }

  private val routesNg: Routes[Any, ZNothing, EndpointMiddleware.None] =
    rootRoute ++ landingRoute ++ yearRoute ++ monthRoute ++ dayRoute

  override protected def run(args: zio.Chunk[String]): ZIO[Any, Throwable, Any] = serve(
    routes.withDefaultErrorResponse // TODO map the errors so that the error message gets to the client
//    routesNg.toApp
  )

  private def renderHtml(content: String): zio.UIO[Response] = ZIO.succeed(Response(
    headers = Headers(Header.ContentType(MediaType.text.html)), // TODO UTF-8?
    body = Body.fromString(content)
  ))

  private def calendarCodec: HttpCodec[HttpCodecType.Path, Calendar] = HttpCodec.string("calendar").transform[Calendar](
    getCalendar,
    _.name
  )

  private def getCalendar(kindStr: String): Calendar = Seq(Jewish, Gregorian)
    .find(_.name == kindStr)
    .getOrElse(throw IllegalArgumentException(s"Unrecognized kind $kindStr"))

  private def getYear(calendar: Calendar, yearStr: String): calendar.Year = calendar.Year(yearStr.toInt)
