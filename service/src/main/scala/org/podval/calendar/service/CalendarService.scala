package org.podval.calendar.service

import cats.effect._
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers._
import org.http4s.MediaType
import org.http4s.server.blaze._
import org.http4s.server.middleware.AutoSlash
import org.podval.calendar.dates.{Calendar, DayBase, MonthBase, YearBase}
import org.podval.calendar.gregorian.Gregorian
import org.podval.calendar.jewish.Jewish
import org.podval.calendar.schedule.tanach.{Reading, Schedule}
import org.podval.judaica.metadata.{Language, LanguageSpec, WithNames}
import org.podval.judaica.metadata.tanach.{Custom, Torah}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try
import scalatags.Text.TypedTag
import scalatags.Text.all._

// TODO around Jewish year 3761, Gregorian day numbers become negative...
// TODO add borders to tables
object CalendarService extends StreamApp[IO] {

  private val calendarService: HttpService[IO] = HttpService[IO] {
    case GET -> Root => renderHtml(renderRoot)

    case GET -> Root / "jewish"
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderLanding(Calendar.nowJewish)(JewishK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "jewish" / JewishYearVar(year)
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderYear(year)(JewishK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "jewish" / JewishYearVar(year) / monthStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderMonth(getJewishMonth(year, monthStr))(JewishK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "jewish" / JewishYearVar(year) / monthStr / IntVar(dayNumber)
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val day = getJewishMonth(year, monthStr).day(dayNumber)
      renderHtml(renderDay(day, Calendar.fromJewish(day))(JewishK, toLocation(maybeLocation), spec = toSpec(maybeLanguage)))

    case GET -> Root / "gregorian"
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderLanding(Calendar.nowGregorian)(GregorianK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "gregorian" / GregorianYearVar(year)
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderYear(year)(GregorianK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "gregorian" / GregorianYearVar(year) / month
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      renderHtml(renderMonth(getGregorianMonth(year, month))(GregorianK, toLocation(maybeLocation), toSpec(maybeLanguage)))

    case GET -> Root / "gregorian" / GregorianYearVar(year) / month / IntVar(dayNumber)
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val day = getGregorianMonth(year, month).day(dayNumber)
      renderHtml(renderDay(Calendar.toJewish(day), day)(GregorianK, toLocation(maybeLocation), toSpec(maybeLanguage)))
  }

  private abstract class Kind(name: String) {
    final override def toString: String = name

    def theOther: Kind
  }

  private final case object JewishK extends Kind("jewish") {
    override def theOther: Kind = GregorianK
  }

  private final case object GregorianK extends Kind("gregorian") {
    override def theOther: Kind = JewishK
  }

  private implicit val languageQueryParamDecoder: QueryParamDecoder[Language] =
    QueryParamDecoder[String].map(Language.getForName)

  private object OptionalLanguageQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Language]("lang")

  private abstract class Location(val name: String, val inHolyLand: Boolean)
  private final case object HolyLand extends Location("Holy Land", true)
  private final case object Diaspora extends Location("Diaspora", false)

  private implicit val locationQueryParamDecoder: QueryParamDecoder[Location] =
    QueryParamDecoder[Boolean].map(if (_) HolyLand else Diaspora)

  private object OptionalInHolyLandQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Location]("inHolyLand")

  private object JewishYearVar {
    def unapply(str: String): Option[Jewish.Year] =
      if (str.isEmpty) None else Try(Jewish.Year(str.toInt)).toOption
  }

  private def getJewishMonth(year: Jewish.Year, monthStr: String): Jewish.Month = {
    val monthName: Option[Jewish.Month.Name] = Jewish.Month.Name.forName(monthStr)
    if (monthName.isDefined) year.month(monthName.get)
    else year.month(monthStr.toInt)
  }

  private object GregorianYearVar {
    def unapply(str: String): Option[Gregorian.Year] =
      if (str.isEmpty) None else Try(Gregorian.Year(str.toInt)).toOption
  }

  private def getGregorianMonth(year: Gregorian.Year, monthStr: String): Gregorian.Month = {
    val monthName: Option[Gregorian.Month.Name] = Gregorian.Month.Name.forName(monthStr)
    if (monthName.isDefined) year.month(monthName.get)
    else year.month(monthStr.toInt)
  }

  private def toSpec(language: Option[Language]): LanguageSpec = language.getOrElse(Language.English).toSpec

  private def toLocation(location: Option[Location]): Location = location.getOrElse(Diaspora)

  private def dayLinks(day: DayBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = span(
    yearLink(day.year),
    monthNameLink(day.month),
    dayLink(day-1, text = Some("<")),
    dayLink(day),
    dayLink(day+1, text = Some(">"))
  )

  private def yearLink(year: YearBase[_], text: Option[String] = None)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = link(
    url = yearUrl(year),
    text = text.getOrElse(spec.toString(year.number))
  )

  private def yearUrl(year: YearBase[_])(implicit kind: Kind): String =
    s"/$kind/${year.number}"

  private def monthLink(month: MonthBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = link(
    url = s"/$kind/${month.year.number}/${month.numberInYear}",
    text = spec.toString(month.numberInYear)
  )

  private def monthNameLink(month: MonthBase[_], text: Option[String] = None)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = link(
    url = monthNameUrl(month),
    text = text.getOrElse(month.name.toLanguageString)
  )

  private def monthNameUrl(month: MonthBase[_])(implicit kind: Kind, spec: LanguageSpec): String =
    s"/$kind/${month.year.number}/${month.name.toLanguageString}"

  private def dayLink(day: DayBase[_], text: Option[String] = None)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = link(
    url = dayUrl(day),
    text = text.getOrElse(spec.toString(day.numberInMonth))
  )

  private def dayUrl(day: DayBase[_])(implicit kind: Kind, spec: LanguageSpec): String =
    s"/$kind/${day.year.number}/${day.month.numberInYear}/${day.numberInMonth}"

  private def link(url: String, text: String)(implicit
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = a(href := s"$url$suffix")(spacing, text)

  private def renderRoot: TypedTag[String] = div(
    div(a(href := "/jewish")("jewish")),
    div(a(href := "/gregorian")("gregorian"))
  )

  private def renderLanding(day: DayBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = addPickers(s"/$kind", dayLinks(day))

  private def renderYear(year: YearBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = addPickers(yearUrl(year), div(
    yearLink(year-1, text = Some("<")),
    yearLink(year),
    yearLink(year+1, text = Some(">")),
    table(
      tbody(
        year.months.map { month: MonthBase[_] =>
          tr(
            td(monthLink(month)),
            td(monthNameLink(month))
          )
        }
      )
    )
  ))

  private def renderMonth(month: MonthBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = addPickers(monthNameUrl(month), div(
    yearLink(month.year),
    monthNameLink(month-1, text = Some("<")),
    monthNameLink(month),
    monthNameLink( month+1, text = Some(">")),
    table(
      tbody(
        month.days.map { day: DayBase[_] =>
          tr(td(dayLink(day)))
        }
      )
    )
  ))

  private def renderDay(day: Jewish.Day, gregorianDay: Gregorian.Day)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    val schedule = Schedule(from = day, to = day, inHolyLand = location.inHolyLand)
    val daySchedule = schedule.days(day)

    val first: DayBase[_] = if (kind == JewishK) day else gregorianDay
    val second: DayBase[_] = if (kind == JewishK) gregorianDay else day

    addPickers(dayUrl(day), div(
      div(dayLinks(first)(kind, location, spec), " ", first.name.toLanguageString),
      div(dayLinks(second)(kind.theOther, location, spec), " ", second.name.toLanguageString),
      div(daySchedule.dayNames.map { withNames: WithNames => span(spacing, withNames.names.doFind(spec).name) }),
      renderOptionalReading("Morning", daySchedule.morning),
      div(
        h5("Chitas"),
        daySchedule.chitas.map(fragment => span(spacing, fragment.toLanguageString))
      ),
      renderOptionalReading("Afternoon", daySchedule.afternoon)
    ))
  }

  private def renderOptionalReading(
    name: String,
    reading: Option[Reading]
  )(implicit
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    def renderByCustom[T](customs: Custom.Of[T])(renderer: T => TypedTag[String]): TypedTag[String] = table(
      tbody(
        customs.customs.toSeq.map { case (custom: Custom, value: T) =>
          tr(td(custom.toLanguageString), td(renderer(value)))
        }
      )
    )

    div(
      h4(name),
      reading.fold(p("None")) { reading => div(
        h5("Torah"),
        div(reading.torah.customs.toSeq.map { case (custom: Custom, torah: Torah) => renderTorah(custom, torah) }),
        h5("Maftir"),
        renderByCustom(reading.maftir)(maftir => span(maftir.fold("None")(_.toLanguageString))),
        h5("Haftarah"),
        renderByCustom(reading.haftarah)(haftarah => span(haftarah.fold("None")(_.toLanguageString)))
      )}
    )
  }

  private def renderTorah(custom: Custom, torah: Torah)(implicit spec: LanguageSpec): TypedTag[String] = table(
    caption(custom.toLanguageString),
    tbody(
      torah.spans.zipWithIndex map { case (reading, index) =>
        tr(td(index+1), td(reading.toLanguageString))
      }
    )
  )

  private def suffix(implicit location: Location, spec: LanguageSpec): String =
    s"?inHolyLand=${location.inHolyLand}&lang=${spec.languageName}"

  private def addPickers(
    url: String,
    content: TypedTag[String]
  )(implicit
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    val languages = Language.values.map(_.toSpec) map { spec1 =>
      val languageName = spec1.languageName
      if (spec1.language == spec.language) span(spacing, languageName)
      else a(href := s"$url${suffix(location, spec1)}")(spacing, languageName)
    }

    val locations = Seq(HolyLand, Diaspora).map { location1 =>
      if (location1 == location) span(spacing, location1.name)
      else a(href := s"$url${suffix(location1, spec)}")(spacing, location1.name)
    }

    div(
      div(languages),
      div(locations),
      content
    )
  }

  private def renderHtml(tag: TypedTag[String]): IO[Response[IO]] =
    Ok(tag.render).map(
      _.withContentType(`Content-Type`(MediaType.`text/html`, Charset.`UTF-8`))
//        .putHeaders(`Cache-Control`(NonEmptyList.of(`no-cache`())))
    )

  private val spacing: Modifier = modifier(
    paddingRight := 10
  )

  // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
  private val builder: BlazeBuilder[IO] = BlazeBuilder[IO].bindHttp(host = "0.0.0.0", port = 8090)
    .mountService(AutoSlash(calendarService), prefix = "/")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    builder.serve
}
