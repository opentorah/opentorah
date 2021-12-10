package org.opentorah.calendar.service

import org.opentorah.calendar.{Calendar, YearsCycle}
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, Season, Shemittah, SpecialDay, Sun, YearType}
import org.opentorah.calendar.roman.Gregorian
import org.opentorah.html.A
import org.opentorah.metadata.{Language, Named, Numbered}
import org.opentorah.schedule.rambam.RambamSchedule
import org.opentorah.schedule.tanach.{Chitas, Schedule}
import org.opentorah.texts.rambam.{MishnehTorah, SeferHamitzvosLessons}
import org.opentorah.texts.tanach.{Custom, Haftarah, Reading, Span, Torah}
import org.opentorah.texts.tanach.Tanach.Psalms
import org.opentorah.util.Collections
import org.opentorah.xml.{PrettyPrinter, ScalaXml}
import Renderer.Location

// TODO there was supposed to be a way to avoid passing context parameters everywhere...
sealed abstract class Renderer:
  val name: String
  val calendar: Calendar
  val other: Renderer

  private final def getYear(yearStr: String): calendar.Year = calendar.Year(yearStr.toInt)

  private final def getMonth(yearStr: String, monthStr: String): calendar.Month =
    val year = getYear(yearStr)
    val monthName: Option[calendar.Month.Name] = calendar.Month.forName(monthStr)
    if monthName.isDefined then year.month(monthName.get)
    else year.month(monthStr.toInt)

  protected def jewish(day: calendar.Day): Jewish.Day

  //  private def toLanguageString(what: LanguageString): String = what.toLanguageString(spec)

  private def yearUrl(year: calendar.Year): Seq[String] =
    Seq(name, year.number.toString)

  private def monthUrl(month: calendar.Month): Seq[String] =
    Seq(name, month.year.number.toString, month.numberInYear.toString)

  private def monthNameUrl(month: calendar.Month)(using location: Location, spec: Language.Spec): Seq[String] =
    Seq(name, month.year.number.toString, month.name.toLanguageString)

  private def dayUrl(day: calendar.Day): Seq[String] =
    Seq(name, day.year.number.toString, day.month.numberInYear.toString, day.numberInMonth.toString)

  private def yearLink(year: calendar.Year, text: Option[String] = None)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    navLink(yearUrl(year), text.getOrElse(year.toLanguageString))

  private def monthLink(month: calendar.Month)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    navLink(monthUrl(month), month.numberInYearToLanguageString)

  private def monthNameLink(month: calendar.Month, text: Option[String] = None)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    navLink(monthNameUrl(month), text.getOrElse(month.name.toLanguageString))

  private def dayLink(day: calendar.Day, text: Option[String] = None)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    navLink(dayUrl(day), text.getOrElse(day.numberInMonthToLanguageString))

  private def navLink(url: Seq[String], text: String)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    A(url).setQuery(suffix).addClass("nav")(text)

  private def suffix(using location: Location, spec: Language.Spec): String = Renderer.suffix(location, spec)

  private def dayLinks(day: calendar.Day)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    <span>
    {yearLink(day.year)}
    {monthNameLink(day.month)}
    {if day.number > 1 then dayLink(day-1, text = Some("<")) else <span>{Renderer.earlyGregorianMessage} </span>}
    {dayLink(day)}
    {dayLink(day+1, text = Some(">"))}
    </span>

  def renderLanding(using location: Location, spec: Language.Spec): String = renderHtml(
    Seq(name),
    dayLinks(Gregorian.now.to(calendar).day)
  )

  def renderYear(yearStr: String)(using location: Location, spec: Language.Spec): String =
    val year: calendar.Year = getYear(yearStr)
    renderHtml(
      yearUrl(year),
      <div>
        {yearLink(year-1, text = Some("<"))}
        {yearLink(year)}
        {yearLink(year+1, text = Some(">"))}
        <table>
          <tbody>
            {year.months.map((month: calendar.Month) =>
            <tr>
              <td>{monthLink(month)}</td>
              <td>{monthNameLink(month)}</td>
            </tr>
          )}
          </tbody>
        </table>
        {renderYearInformation(year)}
      </div>
    )

  protected def renderYearInformation(year: calendar.Year)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] = Seq.empty

  def renderMonth(yearStr: String, monthStr: String)(using location: Location, spec: Language.Spec): String =
    val month: calendar.Month = getMonth(yearStr, monthStr)
    renderHtml(monthNameUrl(month),
      <div>
      {yearLink(month.year)}
      {monthNameLink(month-1, text = Some("<"))}
      {monthNameLink(month)}
      {monthNameLink(month+1, text = Some(">"))}
      <table>
        <tbody>
        {month.days.map((day: calendar.Day) => <tr><td>{dayLink(day)}</td></tr>)}
        </tbody>
      </table>
      </div>
    )

  def renderDay(yearStr: String, monthStr: String, dayStr: String)(using location: Location, spec: Language.Spec): String =
    val day: calendar.Day = getMonth(yearStr, monthStr).day(dayStr.toInt)
    val jewishDay: Jewish.Day = jewish(day)

    val thisDay :       calendar.Day = day
    val otherDay: other.calendar.Day = day.to(other.calendar)

    val daySchedule = Schedule.get(jewishDay, inHolyLand = location.inHolyLand)

    renderHtml(dayUrl(thisDay),
      <div>
      <div>{this .dayLinks(thisDay )} {thisDay .name.toLanguageString}</div>
      <div>{other.dayLinks(otherDay)} {otherDay.name.toLanguageString}</div>
      <div>{daySchedule.dayNames.map((named: Named) =>
        <span class="name">{named.names.doFind(spec).name}</span>)}</div>
      {renderOptionalReading("Morning", daySchedule.morning)}
      {renderOptionalReading("Purim morning alternative", daySchedule.purimAlternativeMorning)}
      {if !jewishDay.isShabbosMevarchim then Seq.empty[ScalaXml.Element] else renderShabbosMevarchim(jewishDay.month.next)}
      <span class="heading">Chitas</span>
      {renderChitas(daySchedule.chitas)}
      <span class="heading">Tehillim</span>
      {renderTehillim(jewishDay)}
      <div class="heading">Rambam</div>
      {renderRambam(RambamSchedule.forDay(jewishDay))}
      {renderOptionalReading("Afternoon", daySchedule.afternoon)}
      </div>
    )

  private def renderShabbosMevarchim(month: Jewish.Month)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] = Seq(
    <span class="subheading">Shabbos Mevarchim</span>,
    <table><tr>
      <td>{month.name.toLanguageString}</td>
      {if month.prev.length == 30 then <td>{month.prev.lastDay.name.toLanguageString}</td> else <td/>}
      <td>{month.firstDay.name.toLanguageString}</td>
      <td>{month.newMoon.toLanguageString}</td>
    </tr></table>
  )

  private def renderChitas(chitas: Chitas)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    def renderFragment(fragment: Torah.Fragment): Seq[ScalaXml.Element] = Seq(
      <td><span>{fragment.toLanguageString}</span></td>,
      <td>{renderSource(fragment.source)}</td>
    )

    <table><tbody>
      <tr>{renderFragment(chitas.first)}</tr> +:
      {chitas.second.fold(Seq.empty[ScalaXml.Element])(fragment => Seq(<tr>{renderFragment(fragment)}</tr>))}
    </tbody></table>

  private def renderTehillim(day: Jewish.Day)(using location: Location, spec: Language.Spec): ScalaXml.Element =
    val forDayOfMonth: Span =
      if (day.numberInMonth == 29) && day.month.length == 29 then Span(Psalms.days(29-1).from, Psalms.days(30-1).to)
      else Psalms.days(day.numberInMonth-1)

    val forDayOfWeek: Span =
      Psalms.weekDays(day.numberInWeek-1)

    <table>
      <tr><td>for day of month</td><td><span>{forDayOfMonth.toLanguageString}</span></td></tr>
      <tr><td>for day of week</td><td><span>{forDayOfWeek.toLanguageString}</span></td></tr>
    </table>

  private def renderRambam(schedule: RambamSchedule)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] = Seq(
    <span class="subheading">3 chapters</span>,
    <table>
      <tr><td>Cycle</td><td>Lesson</td></tr>
      <tr>
        <td>{spec.toString(schedule.threeChapters.cycle)}</td>
        <td>{spec.toString(schedule.threeChapters.lesson)}</td>
      </tr>
    </table>,
    <span class="subheading">chapters</span>,
    <table>
      <tr><td/><td>Book</td><td>Part</td><td>Chapter</td></tr>
      <tr><td>{spec.toString(1)}</td>{renderRambamChapter(schedule.threeChapters.chapter1)}</tr>
      <tr><td>{spec.toString(2)}</td>{renderRambamChapter(schedule.threeChapters.chapter2)}</tr>
      <tr><td>{spec.toString(3)}</td>{renderRambamChapter(schedule.threeChapters.chapter3)}</tr>
    </table>,
    <span class="subheading">Sefer Hamitzvos</span>,
    <table>{schedule.seferHamitzvos.parts.map((part: SeferHamitzvosLessons.Part) =>
      <tr><td>{part.toLanguageString}</td></tr>
    )}</table>,
    <span class="subheading">1 chapter</span>,
    <table>
      <tr><td>Cycle</td><td>Year</td><td>Chapter number</td></tr>
      <tr>
        <td>{spec.toString(schedule.oneChapter.cycle)}</td>
        <td>{spec.toString(schedule.oneChapter.year)}</td>
        <td>{spec.toString(schedule.oneChapter.chapterNumber)}</td>
      </tr>
    </table>,
    <span class="subheading">chapter</span>,
    <table>
      <tr><td>Book</td><td>Part</td><td>Chapter</td></tr>
      <tr>{renderRambamChapter(schedule.oneChapter.chapter)}</tr>
    </table>
  )

  private def renderRambamChapter(chapter: MishnehTorah.Chapter)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] = Seq(
    <td>{chapter.part.book.toLanguageString}</td>,
    <td>{chapter.part.toLanguageString}</td>,
    <td>{chapter.toLanguageString}</td>
  )

  private def renderOptionalReading(name: String, reading: Option[Reading])(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    reading.fold(Seq.empty[ScalaXml.Element])(reading => Seq(<div>
      <span class="heading">{name}</span>
      {
        val maftirCommonOnly = reading.maftir.commonOnly
        val haftarahCommonOnly = reading.haftarah.commonOnly
        val noMaftirHaftarah: Boolean =
          maftirCommonOnly.fold(false)(_.isEmpty) && haftarahCommonOnly.fold(false)(_.isEmpty)
        val varyingMaftirAndHaftarah: Boolean = maftirCommonOnly.isEmpty && haftarahCommonOnly.isEmpty

        <div>
          {renderCustoms("Torah", reading.torah, renderTorah)}
          {if noMaftirHaftarah then Seq.empty[ScalaXml.Element] else
          if varyingMaftirAndHaftarah then
            renderCustoms("Maftir and Haftarah", reading.maftirAndHaftarah, renderMaftirAndHaftarah)
          else
            renderCustoms("Maftir", reading.maftir, renderMaftir) ++
            renderCustoms("Haftarah", reading.haftarah, renderHaftarah)
          }
        </div>
      }
    </div>))

  private def renderTorah(torah: Torah)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    torah.spans.zipWithIndex map((fragment, index) =>
      <tr>
        <td>{spec.toString(index + 1)}</td>
        <td>{fragment.toLanguageString}</td>
        <td>{renderSource(fragment.source)}</td>
      </tr>
    )

  private def renderMaftir(maftir: Option[Torah.Maftir])(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    Seq(maftir.fold(<tr><td>None</td></tr>)(maftir =>
      <tr>
        <td>{maftir.toLanguageString}</td>
        <td>{renderSource(maftir.source)}</td>
      </tr>
    ))

  private def renderHaftarah(haftarah: Option[Haftarah])(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    haftarah.fold(Seq(<tr><td>None</td></tr>))(haftarah =>
      val spans = haftarah.spans
      // TODO sources should be more precise than just Named: eq equality is not right in the general case...
      given CanEqual[Named, Named] = CanEqual.derived
      val parts: Seq[Seq[Haftarah.BookSpan]] =
        Collections.group[Haftarah.BookSpan, Option[Named]](spans, span => span.source)
      parts.map((part: Seq[Haftarah.BookSpan]) =>
        <tr>
          <td>{Haftarah.toLanguageString(part)}</td>
          <td>{renderSource(part.head.source)}</td>
        </tr>
      ))

  private def renderMaftirAndHaftarah(maftirAndHaftarah: Option[Reading.MaftirAndHaftarah])(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    renderMaftir(maftirAndHaftarah.flatMap(_.maftir)) ++
      renderHaftarah(maftirAndHaftarah.map(_.haftarah))

  private def renderSource(source: Option[Named])(using location: Location, spec: Language.Spec): String =
    source.fold[String]("")(_.toLanguageString)

  private def renderCustoms[T](
    what: String,
    customs: Custom.Of[T],
    renderer: T => Seq[ScalaXml.Element]
  )(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
    <span class="subheading">{what}</span> +:
    customs.customs.toSeq.map((custom: Custom, valueForCustom /*: T*/) =>
      <table class="custom">
        <caption>{custom.toLanguageString}</caption>
        <tbody>{renderer(valueForCustom)}</tbody>
      </table>
    )

  private def renderHtml(url: Seq[String], content: ScalaXml.Element)(using location: Location, spec: Language.Spec): String =
    Renderer.renderHtml(url, content, location, spec)

object Renderer:
  enum Location(val name: String, val inHolyLand: Boolean) derives CanEqual:
    case HolyLand extends Location("Holy Land", true)
    case Diaspora extends Location("Diaspora", false)

  def getLocation(parameter: Option[String]): Location =
    val holyLand: Boolean = parameter.forall(_ == "true")
    if holyLand then Location.HolyLand else Location.Diaspora
  
  private val earlyGregorianMessage: String = "Gregorian dates before year 1 are not supported!"

  object JewishRenderer extends Renderer:
    override val name: String = "jewish"
    override val calendar: Calendar = Jewish
    override val other: Renderer = GregorianRenderer

    override protected def jewish(day: calendar.Day): Jewish.Day = day.asInstanceOf[Jewish.Day]

    override protected def renderYearInformation(yearRaw: calendar.Year)(using location: Location, spec: Language.Spec): Seq[ScalaXml.Element] =
      val year: Jewish.Year = yearRaw.asInstanceOf[Jewish.Year]
      val delay = year.newYearDelay

      val numbers: ScalaXml.Element =
        <table>
          <tr><td>from creation</td><td>{year.toLanguageString}</td></tr>
          <tr><td>is leap</td><td>{year.isLeap.toString}</td></tr>
          <tr><td>months</td><td>{spec.toString(year.lengthInMonths)}</td></tr>
          <tr><td>days</td><td>{spec.toString(year.lengthInDays)}</td></tr>
          <tr><td>type</td><td>{YearType.forYear(year).toString}</td></tr>
          <tr><td>Molad</td><td>{year.newMoon.toLanguageString}</td></tr>
          <tr><td>New Year Delay</td><td>{s"$delay (${delay.days})"}</td></tr>
        </table>

      def cycle(name: String, yearsCycle: YearsCycle): ScalaXml.Element =
        val in = yearsCycle.forYear(Jewish)(year)
        <tr>
          <td>{name}</td>
          <td>{spec.toString(in.cycleNumber)}</td>
          <td>{spec.toString(in.numberInCycle)}</td>
        </tr>

      val cycles: ScalaXml.Element =
        <table>
          <tr><td>Cycle"</td><td>Number</td><td>In Cycle"</td></tr>
          {cycle("Leap Years", LeapYearsCycle)}
          {cycle("Shemittah", Shemittah)}
          {cycle("Birchas Hachamo", Sun.Shmuel)}
        </table>

      val tkufot: ScalaXml.Element =
        def tkufa(flavor: Season.ForYear, season: Season): String =
          flavor.seasonForYear(season, year).toLanguageString

        <table>
          <tr><td>Tkufa</td><td>Shmuel</td><td>Rav Ada"</td></tr>
          {Season.valuesSeq.map { season =>
          <tr>
            <td>{season.toLanguageString}</td>
            <td>{tkufa(Sun.Shmuel, season)}</td>
            <td>{tkufa(Sun.RavAda, season)}</td>
          </tr>}}
        </table>

      val festivalDays: Seq[(SpecialDay, Jewish.Day)] =
        SpecialDay.daysWithSpecialReadings(location == Location.HolyLand)
          .map(specialDay => specialDay -> specialDay.correctedDate(year))
          .toSeq.sortBy(_._2)

      val festivals: ScalaXml.Element =
        <table>
        {festivalDays.map((specialDay, day) =>
        <tr>
          <td>{specialDay.toLanguageString}</td>
          <td>{day.toLanguageString}</td>
          <td>{day.name.toLanguageString}</td>
        </tr>)}
      </table>

      Seq(
        <span class="heading">Year</span>,
        numbers,
        <span class="heading">Cycles</span>,
        cycles,
        <span class="heading">Tkufot</span>,
        tkufot,
        <span class="heading">Special Days</span>,
        festivals
      )

  object GregorianRenderer extends Renderer:
    override val name: String = "gregorian"
    override val calendar: Calendar = Gregorian
    override val other: Renderer = JewishRenderer
    override protected def jewish(day: calendar.Day): Jewish.Day = day.to(Jewish)

  def renderRoot(location: Location, spec: Language.Spec): String = renderHtml(
    url = Seq.empty,
    content =
      <div>
        <div>{A(Seq(JewishRenderer.name))(text = "jewish")}</div>,
        <div>{A(Seq(GregorianRenderer.name))(text = "gregorian")}</div>
      </div>,
    location = location,
    spec = spec
  )

  def renderLanding(kindStr: String, location: Location, spec: Language.Spec): String =
    renderer(kindStr).renderLanding(using location, spec)

  def renderYear(kindStr: String, location: Location, spec: Language.Spec, yearStr: String): String =
    renderer(kindStr).renderYear(yearStr)(using location, spec)

  def renderMonth(kindStr: String, location: Location, spec: Language.Spec, yearStr: String, monthStr: String): String =
    renderer(kindStr).renderMonth(yearStr, monthStr)(using location, spec)

  def renderDay(kindStr: String, location: Location, spec: Language.Spec, yearStr: String, monthStr: String, dayStr: String): String =
    renderer(kindStr).renderDay(yearStr, monthStr, dayStr)(using location, spec)

  private def renderer(kindStr: String): Renderer = Seq(JewishRenderer, GregorianRenderer)
    .find(_.name == kindStr)
    .getOrElse(throw IllegalArgumentException(s"Unrecognized kind $kindStr"))

  private def renderHtml(
    url: Seq[String],
    content: ScalaXml.Element,
    location: Location,
    spec: Language.Spec
  ): String =
    val languages: Seq[ScalaXml.Element] = Language.valuesSeq.map(_.toSpec).map(spec1 =>
      val languageName = spec1.languageName
      if spec1.language == spec.language then <span class="picker">{languageName}</span>
      else A(url).setQuery(suffix(location, spec1)).addClass("picker")(text = languageName)
    )

    val locations: Seq[ScalaXml.Element] = Seq(Location.HolyLand, Location.Diaspora).map(location1 =>
      if location1 == location then <span class="picker">{location1.name}</span>
      else A(url).setQuery(suffix(location1, spec)).addClass("picker")(text = location1.name)
    )

    //        title("Reading Schedule")?
    val result =
      <html dir={direction(spec)}>
        <head>
          <link rel="stylesheet" type="text/css" href="/style.css"/>
        </head>
        <body>
          {languages}
          {locations}
          {content}
        </body>
      </html>

    PrettyPrinter.default.render(ScalaXml)(result)

  private def direction(spec: Language.Spec): String =
    if spec.language.contains(Language.Hebrew) then "rtl" else "ltr" // TODO add a method to Language...

  private def suffix(location: Location, spec: Language.Spec): String =
    s"inHolyLand=${location.inHolyLand}&lang=${spec.languageName}"
