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
      renderDay(kind.jewish(day), kind.gregorian(day))(kind, toLocation(maybeLocation), toSpec(maybeLanguage))
  }

  private abstract class Kind(val name: String) {
    final override def toString: String = name

    def now: DayBase[_]

    def getYear(yearStr: String): YearBase[_]

    def getMonth(yearStr: String, monthStr: String): MonthBase[_]

    def jewish(day: DayBase[_]): Jewish.Day

    def gregorian(day: DayBase[_]): Gregorian.Day

    def theOther: Kind

    def yearNumberToString(number: Int)(implicit spec: LanguageSpec): String

    def monthNumberToString(number: Int)(implicit spec: LanguageSpec): String

    def dayNumberToString(number: Int)(implicit spec: LanguageSpec): String
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
    override def jewish(day: DayBase[_]): Jewish.Day = day.asInstanceOf[Jewish.Day]
    override def gregorian(day: DayBase[_]): Gregorian.Day = Calendar.fromJewish(jewish(day))

    override def theOther: Kind = GregorianK

    override def yearNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      spec.toString(number)

    override def monthNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      spec.toString(number)

    override def dayNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      spec.toString(number)
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
    override def jewish(day: DayBase[_]): Jewish.Day = Calendar.toJewish(gregorian(day))
    override def gregorian(day: DayBase[_]): Gregorian.Day = day.asInstanceOf[Gregorian.Day]
    override def theOther: Kind = JewishK

    override def yearNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      number.toString

    override def monthNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      number.toString

    override def dayNumberToString(number: Int)(implicit spec: LanguageSpec): String =
      number.toString
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
  ): TypedTag[String] = {
    val label: String = text.getOrElse(kind.yearNumberToString(year.number))
    a(cls := "nav", href := s"${yearUrl(year)}$suffix")(label)
  }

  private def yearUrl(year: YearBase[_])(implicit kind: Kind): String =
    s"/$kind/${year.number}"

  // TODO monthUrl
  private def monthLink(month: MonthBase[_])(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    val label: String = kind.monthNumberToString(month.numberInYear)
    a(cls := "nav", href := s"/$kind/${month.year.number}/${month.numberInYear}/$suffix")(label)
  }

  private def monthNameLink(month: MonthBase[_], text: Option[String] = None)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    val label: String = text.getOrElse(month.name.toLanguageString)
    a(cls := "nav", href := s"${monthNameUrl(month)}$suffix")(label)
  }

  private def monthNameUrl(month: MonthBase[_])(implicit kind: Kind, spec: LanguageSpec): String =
    s"/$kind/${month.year.number}/${month.name.toLanguageString}"

  private def dayLink(day: DayBase[_], text: Option[String] = None)(implicit
    kind: Kind,
    location: Location,
    spec: LanguageSpec
  ): TypedTag[String] = {
    val label: String = text.getOrElse(kind.dayNumberToString(day.numberInMonth))
    a(cls := "nav", href := s"${dayUrl(day)}$suffix")(label)
  }

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

  private def direction(implicit spec: LanguageSpec): String =
    if (spec.language.contains(Language.Hebrew)) "rtl" else "ltr"

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

    renderHtml(dayUrl(first), div(
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
      span(renderNames(fragment.names), fragment.torah.toLanguageString)

    div(
      span(cls := "heading")("Chitas"),
      renderFragment(chitas.first),
      chitas.second.fold(Seq.empty[TypedTag[String]])(fragment => Seq(renderFragment(fragment)))
    )
  }

  private def renderNames(names: Names)(implicit spec: LanguageSpec): TypedTag[String] =
    span(cls := "name", names.doFind(spec).name)

  private def renderOptionalReading(
    name: String,
    reading: Option[Reading]
  )(implicit
    location: Location,
    spec: LanguageSpec
  ): Seq[TypedTag[String]] = {
    def renderByCustom[T <: LanguageString](title: String, customs: Custom.Of[Option[T]]): Seq[TypedTag[String]] = Seq(
      span(cls := "subheading")(title),
      table(tbody(
        customs.customs.toSeq.map { case (custom: Custom, value: Option[T]) =>
          tr(td(custom.toLanguageString), td(span(value.fold("None")(_.toLanguageString))))
        }
      ))
    )

    reading.fold(Seq.empty[TypedTag[String]]) { reading => Seq(div(
      span(cls := "heading")(name),
      {
        val torah: Seq[TypedTag[String]] = reading.torah.customs.toSeq.map { case (custom: Custom, torah: Torah) =>
          div(cls := "custom")(table(
            caption(custom.toLanguageString),
            tbody(
              torah.spans.zipWithIndex map { case (fragment, index) =>
                tr(td(spec.toString(index+1)), td(fragment.toLanguageString))
              }
            )
          ))
        }

        val maftir = reading.maftir
        val haftarah = reading.haftarah
        val noMaftirHaftarah: Boolean =
          maftir.commonOnly.fold(false)(_.isEmpty) && haftarah.commonOnly.fold(false)(_.isEmpty)

        div(
          span(cls := "subheading")("Torah"),
          reading.names.map(renderNames).toSeq,
          torah,
          if (noMaftirHaftarah) Seq.empty[TypedTag[String]] else Seq(
            renderByCustom("Maftir", maftir),
            renderByCustom("Haftarah", haftarah)
          )
        )
      }
    ))}
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
      if (spec1.language == spec.language) span(cls := "picker", languageName)
      else a(cls := "picker", href := s"$url${suffix(location, spec1)}")(languageName)
    }

    val locations = Seq(HolyLand, Diaspora).map { location1 =>
      if (location1 == location) span(cls := "picker", location1.name)
      else a(cls := "picker", href := s"$url${suffix(location1, spec)}")(location1.name)
    }

    val result = html(dir := direction)(
      head(
//        title("Reading Schedule"),
        link(rel := "stylesheet", `type` := "text/css", href := "/style.css")
      ),
      body(
        div(
          div(languages),
          div(locations),
          content
        )
      )
    )

    Ok(result.render).map(
      _.withContentType(`Content-Type`(MediaType.`text/html`, Charset.`UTF-8`))
    )
  }

  private def suffix(implicit location: Location, spec: LanguageSpec): String =
    s"?inHolyLand=${location.inHolyLand}&lang=${spec.languageName}"

  // To be accessible when running in a docker container the server must bind to all IPs, not just 127.0.0.1:
  private val builder: BlazeBuilder[IO] = BlazeBuilder[IO]
    .bindHttp(host = "0.0.0.0", port = getServicePort)
    .mountService(AutoSlash(calendarService), prefix = "/")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] =
    builder.serve

  private def getServicePort: Int =
    scala.util.Properties.envOrNone("SERVICE_PORT").map(_.toInt).getOrElse(8090)
}
