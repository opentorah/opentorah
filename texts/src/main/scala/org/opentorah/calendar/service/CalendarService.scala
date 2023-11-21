package org.opentorah.calendar.service

import org.opentorah.calendar.Calendar
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.roman.Gregorian
//import org.opentorah.metadata.Language
import org.opentorah.service.{ServiceApp, Static}
import ServiceApp.given
import zio.http.codec.PathCodec
import zio.http.codec.PathCodec.empty
//import zio.http.endpoint.{Endpoint, EndpointMiddleware}
import zio.http.{Body, Handler, handler, Header, Headers, MediaType, Method, Request, Response, Routes, string}
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

  private val staticResourceExtensions: Seq[String] = Seq(".ico", ".css", ".js")

  private def routes = Routes(
    Method.GET / empty ->
      handler { (request: Request) =>
        renderHtml(Renderer.renderRoot(Location.fromRequest(request), Lang.fromRequest(request)))
      },

    Method.GET / string("calendarStr") ->
      handler { (calendarStr: String, request: Request) =>
        renderHtml(Renderer(CalendarService.getCalendar(calendarStr), Location.fromRequest(request), Lang.fromRequest(request)).renderLanding)
      },

    Method.GET / string("calendarStr") / string("year") ->
      handler { (calendarStr: String, year: String, request: Request) =>
        val calendar: Calendar = CalendarService.getCalendar(calendarStr)
        renderHtml(Renderer(calendar, Location.fromRequest(request), Lang.fromRequest(request)).renderYear(year))
      },

    Method.GET / string("calendarStr") / string("year") / string("month") ->
      handler { (calendarStr: String, year: String, month: String, request: Request) =>
        val calendar: Calendar = CalendarService.getCalendar(calendarStr)
        renderHtml(Renderer(calendar, Location.fromRequest(request), Lang.fromRequest(request)).renderMonth(year, month))
      },

    Method.GET / string("calendarStr") / string("year") / string("month") / string("day") ->
      handler { (calendarStr: String, year: String, month: String, day: String, request: Request) =>
        val calendar: Calendar = CalendarService.getCalendar(calendarStr)
        renderHtml(Renderer(calendar, Location.fromRequest(request), Lang.fromRequest(request)).renderDay(year, month, day))
      },

    // TODO
//    Method.GET ->
//      handler { (request: Request) =>
//        val path = request.path
//        if staticResourceExtensions.exists(path.segments.last.endsWith)
//        then ServiceApp.orNotFound(path.toString, Static.resource("/" + path, Some(request)))
//        else Handler.notFound
//      }
  )

// TODO NG
//  private val rootEndpoint = Endpoint
//    .get(HttpCodec.empty)
//    .query(Location.codec)
//    .query(Lang.codec)
//    .out[String]
//
//  private val calendarEndpoint = Endpoint
//    .get(CalendarService.calendarCodec)
//    .query(Location.codec)
//    .query(Lang.codec)
//    .out[String]
//
//  private val landingEndpoint =
//    calendarEndpoint
//
//  private val yearCodec : PathCodec[String] = HttpCodec.string("year" )
//  private val monthCodec: PathCodec[String] = HttpCodec.string("month")
//  private val dayCodec  : PathCodec[String] = HttpCodec.string("day"  )
//
//  private val yearEndpoint =
//    calendarEndpoint.path(yearCodec)
//
//  private val monthEndpoint =
//    calendarEndpoint.path(yearCodec / monthCodec)
//
//  private val dayEndpoint =
//    calendarEndpoint.path(yearCodec / monthCodec / dayCodec)
//
//  private val rootRoute: Route[Any, Nothing] = rootEndpoint.implement {
//    case (location: Location, language: Language.Spec) =>
//      ZIO.succeed(Renderer.renderRoot(location, language))
//  }
//
//  private val landingRoute = landingEndpoint.implement {
//    case (calendar: Calendar, location: Location, language: Language.Spec) =>
//      ZIO.succeed(Renderer(calendar, location, language).renderLanding)
//  }
//
//  private val yearRoute = yearEndpoint.implement {
//    case (calendar: Calendar, location: Location, language: Language.Spec, year: String) =>
//      ZIO.succeed(Renderer(calendar, location, language).renderYear(year))
//  }
//
//  private val monthRoute = {
//    case (calendar: Calendar, location: Location, language: Language.Spec, year: String, month: String) =>
//      ZIO.succeed(Renderer(calendar, location, language).renderMonth(year, month))
//  }
//
//  private val dayRoute = {
//    case (calendar: Calendar, location: Location, language: Language.Spec, year: String, month: String, day: String) =>
//      ZIO.succeed(Renderer(calendar, location, language).renderDay(year, month, day))
//  }
//
//  private val routesNg =
//    rootRoute ++ landingRoute ++ yearRoute ++ monthRoute ++ dayRoute

  override protected def run(args: zio.Chunk[String]): ZIO[Any, Throwable, Any] = serve(
    routes.toHttpApp
//    routesNg.toApp
  )

  private def renderHtml(content: String): zio.UIO[Response] = ZIO.succeed(Response(
    headers = Headers(Header.ContentType(MediaType.text.html)), // TODO UTF-8?
    body = Body.fromString(content)
  ))

//  private def calendarCodec: HttpCodec[HttpCodecType.Path, Calendar] = HttpCodec.string("calendar").transform[Calendar](
//    getCalendar,
//    _.name
//  )

  private def getCalendar(kindStr: String): Calendar = Seq(Jewish, Gregorian)
    .find(_.name == kindStr)
    .getOrElse(throw IllegalArgumentException(s"Unrecognized kind $kindStr"))

//  private def getYear(calendar: Calendar, yearStr: String): calendar.Year = calendar.Year(yearStr.toInt)
