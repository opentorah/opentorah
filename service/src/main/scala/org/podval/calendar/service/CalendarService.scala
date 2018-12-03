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
import org.podval.calendar.schedule.tanach.{Chitas, Reading, Schedule}
import org.podval.judaica.metadata.{Language, LanguageSpec, LanguageString, Names, WithNames}
import org.podval.judaica.metadata.tanach.{Custom, Torah}

import scala.concurrent.ExecutionContext.Implicits.global
import scalatags.Text.TypedTag
import scalatags.Text.all._

// TODO around Jewish year 3761, Gregorian day numbers become negative...
// TODO add borders to tables
object CalendarService extends StreamApp[IO] {

  private val calendarService: HttpService[IO] = HttpService[IO] {
    case GET -> Root / "favicon.ico" => NotFound("No favicon.ico")

    case GET -> Root
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    => renderRoot(toLocation(maybeLocation), toSpec(maybeLanguage))

    case GET -> Root / kindStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val kind = getKind(kindStr)
      renderLanding(kind.now)(kind, toLocation(maybeLocation), toSpec(maybeLanguage))

    case GET -> Root / kindStr / yearStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val kind = getKind(kindStr)
      renderYear(kind.getYear(yearStr))(kind, toLocation(maybeLocation), toSpec(maybeLanguage))

    case GET -> Root / kindStr / yearStr / monthStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val kind = getKind(kindStr)
      renderMonth(kind.getMonth(yearStr, monthStr))(kind, toLocation(maybeLocation), toSpec(maybeLanguage))

    case GET -> Root / kindStr / yearStr / monthStr / dayStr
      :? OptionalLanguageQueryParamMatcher(maybeLanguage)
      :? OptionalInHolyLandQueryParamMatcher(maybeLocation)
    =>
      val kind = getKind(kindStr)
      val day: DayBase[_] = kind.getMonth(yearStr, monthStr).day(dayStr.toInt)
      // TODO make renderDay order-independent in its explicit parameters
      renderDay(kind.first(day), kind.second(day))(kind, toLocation(maybeLocation), toSpec(maybeLanguage))
  }

  private abstract class Kind(val name: String) {
    final override def toString: String = name

    def now: DayBase[_]

    def getYear(yearStr: String): YearBase[_]

    def getMonth(yearStr: String, monthStr: String): MonthBase[_]

    def first(day: DayBase[_]): Jewish.Day

    def second(day: DayBase[_]): Gregorian.Day

    def theOther: Kind
  }

  private final case object JewishK extends Kind("jewish") {
    override def now: Jewish.Day = Jewish.now
    override def getYear(yearStr: String): Jewish.Year = Jewish.Year(yearStr.toInt)
    // TODO factor out commonality:
    override def getMonth(yearStr: String, monthStr: String): Jewish.Month = {
      val year = getYear(yearStr)
      val monthName: Option[Jewish.Month.Name] = Jewish.Month.Name.forName(monthStr)
      if (monthName.isDefined) year.month(monthName.get)
      else year.month(monthStr.toInt)
    }
    override def first(day: DayBase[_]): Jewish.Day = day.asInstanceOf[Jewish.Day]
    override def second(day: DayBase[_]): Gregorian.Day = Calendar.fromJewish(first(day))

    override def theOther: Kind = GregorianK
  }

  private final case object GregorianK extends Kind("gregorian") {
    override def now: Gregorian.Day = Gregorian.now
    override def getYear(yearStr: String): Gregorian.Year = Gregorian.Year(yearStr.toInt)
    // TODO factor out commonality:
    override def getMonth(yearStr: String, monthStr: String): Gregorian.Month = {
      val year = getYear(yearStr)
      val monthName: Option[Gregorian.Month.Name] = Gregorian.Month.Name.forName(monthStr)
      if (monthName.isDefined) year.month(monthName.get)
      else year.month(monthStr.toInt)
    }
    override def first(day: DayBase[_]): Jewish.Day = Calendar.toJewish(second(day))
    override def second(day: DayBase[_]): Gregorian.Day = day.asInstanceOf[Gregorian.Day]
    override def theOther: Kind = JewishK
  }

  private def getKind(kindStr: String): Kind =
    if (kindStr == JewishK.name) JewishK else if (kindStr == GregorianK.name) GregorianK else
      throw new IllegalArgumentException(s"Unrecognized kind $kindStr")

  private implicit val languageQueryParamDecoder: QueryParamDecoder[Language] =
    QueryParamDecoder[String].map(Language.getForName)

  private object OptionalLanguageQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Language]("lang")

  private abstract class Location(val name: String, val inHolyLand: Boolean)
  private final case object HolyLand extends Location("Holy Land", true)
  private final case object Diaspora extends Location("Diaspora", false)

  private implicit val locationQueryParamDecoder: QueryParamDecoder[Location] =
    QueryParamDecoder[Boolean].map(if (_) HolyLand else Diaspora)

  private object OptionalInHolyLandQueryParamMatcher extends OptionalQueryParamDecoderMatcher[Location]("inHolyLand")

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
    // TODO monthUrl
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

  private def renderRoot(implicit location: Location, spec: LanguageSpec): IO[Response[IO]] =
    renderHtml("/", div(
      div(a(href := "/jewish")("jewish")),
      div(a(href := "/gregorian")("gregorian"))
    ))

  private def renderLanding(day: DayBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): IO[Response[IO]] = renderHtml(s"/$kind", dayLinks(day))

  private def renderYear(year: YearBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): IO[Response[IO]] = renderHtml(yearUrl(year), div(
    yearLink(year-1, text = Some("<")),
    yearLink(year),
    yearLink(year+1, text = Some(">")),
    table(tbody(year.months.map { month: MonthBase[_] =>
      tr(
        td(monthLink(month)),
        td(monthNameLink(month))
      )
    }))
  ))

  private def renderMonth(month: MonthBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): IO[Response[IO]] = renderHtml(monthNameUrl(month), div(
    yearLink(month.year),
    monthNameLink(month-1, text = Some("<")),
    monthNameLink(month),
    monthNameLink( month+1, text = Some(">")),
    table(tbody(month.days.map { day: DayBase[_] => tr(td(dayLink(day))) }))
  ))

  private def renderDay(day: Jewish.Day, gregorianDay: Gregorian.Day)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): IO[Response[IO]] = {
    val schedule = Schedule(from = day, to = day, inHolyLand = location.inHolyLand)
    val daySchedule = schedule.days(day)

    val first: DayBase[_] = if (kind == JewishK) day else gregorianDay
    val second: DayBase[_] = if (kind == JewishK) gregorianDay else day

    renderHtml(dayUrl(day), div(
      div(dayLinks(first)(kind, location, spec), " ", first.name.toLanguageString),
      div(dayLinks(second)(kind.theOther, location, spec), " ", second.name.toLanguageString),
      div(daySchedule.dayNames.map { withNames: WithNames => renderNames(withNames.names) }),
      renderOptionalReading("Morning", daySchedule.morning),
      renderChitas(daySchedule.chitas),
      renderOptionalReading("Afternoon", daySchedule.afternoon)
    ))
  }

  private def renderChitas(chitas: Chitas)(implicit spec: LanguageSpec): TypedTag[String] = {
    def renderFragment(fragment: Chitas.Fragment): TypedTag[String] =
      span(spacing, renderNames(fragment.names), fragment.torah.toLanguageString)

    div(
      h4("Chitas"),
      renderFragment(chitas.first),
      chitas.second.fold(Seq.empty[TypedTag[String]])(fragment => Seq(renderFragment(fragment)))
    )
  }

  private def renderNames(names: Names)(implicit spec: LanguageSpec): TypedTag[String] =
    span(spacing, names.doFind(spec).name)

  private def renderOptionalReading(
    name: String,
    reading: Option[Reading]
  )(implicit
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    def renderByCustom[T <: LanguageString](title: String, customs: Custom.Of[Option[T]]): Seq[TypedTag[String]] = Seq(
      h5(title),
      table(tbody(
        customs.customs.toSeq.map { case (custom: Custom, value: Option[T]) =>
          tr(td(custom.toLanguageString), td(span(value.fold("None")(_.toLanguageString))))
        }
      ))
    )

    div(
      h4(name),
      reading.fold(p("None")) { reading =>
        val torah: Seq[TypedTag[String]] = reading.torah.customs.toSeq.map { case (custom: Custom, torah: Torah) => table(
          caption(custom.toLanguageString),
          tbody(
            torah.spans.zipWithIndex map { case (fragment, index) =>
              tr(td(index+1), td(fragment.toLanguageString))
            }
          )
        )}

        val maftir = reading.maftir
        val haftarah = reading.haftarah
        val noMaftirHaftarah: Boolean =
          maftir.commonOnly.fold(false)(_.isEmpty) && haftarah.commonOnly.fold(false)(_.isEmpty)

        div(
          h5(Seq[TypedTag[String]](span(spacing, "Torah")) ++ reading.names.map(renderNames).toSeq),
          torah,
          if (noMaftirHaftarah) h5("No maftir/haftarah") else Seq(
            renderByCustom("Maftir", maftir),
            renderByCustom("Haftarah", haftarah)
          )
        )
      }
    )
  }

  private def renderHtml(
    url: String,
    content: TypedTag[String]
  )(implicit
    location: Location,
    spec: LanguageSpec
  ): IO[Response[IO]] = {
    val languages = Language.values.map(_.toSpec) map { spec1 =>
      val languageName = spec1.languageName
      if (spec1.language == spec.language) span(spacing, languageName)
      else a(href := s"$url${suffix(location, spec1)}")(spacing, languageName)
    }

    val locations = Seq(HolyLand, Diaspora).map { location1 =>
      if (location1 == location) span(spacing, location1.name)
      else a(href := s"$url${suffix(location1, spec)}")(spacing, location1.name)
    }

    val result = div(
      div(languages),
      div(locations),
      content
    )

    Ok(result.render).map(
      _.withContentType(`Content-Type`(MediaType.`text/html`, Charset.`UTF-8`))
    )
  }

  private val spacing: Modifier = modifier(
    paddingRight := 10
  )

  private def link(url: String, text: String)(implicit location: Location, spec: LanguageSpec): TypedTag[String] =
    a(href := s"$url$suffix")(spacing, text)

  private def suffix(implicit location: Location, spec: LanguageSpec): String =
    s"?inHolyLand=${location.inHolyLand}&lang=${spec.languageName}"

  // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
  private val builder: BlazeBuilder[IO] = BlazeBuilder[IO].bindHttp(host = "0.0.0.0", port = 8090)
    .mountService(AutoSlash(calendarService), prefix = "/")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    builder.serve
}
