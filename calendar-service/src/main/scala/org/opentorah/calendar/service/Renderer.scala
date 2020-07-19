package org.opentorah.calendar.service

import org.opentorah.calendar.Calendars
import org.opentorah.calendar.gregorian.Gregorian
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, Season, Shemittah, SpecialDay, Sun, YearType}
import org.opentorah.dates.{Calendar, DayBase, MonthBase, YearBase, YearsCycle}
import org.opentorah.metadata.{Language, LanguageSpec, WithNames}
import org.opentorah.schedule.rambam.RambamSchedule
import org.opentorah.schedule.tanach.{Chitas, Schedule}
import org.opentorah.texts.rambam.{MishnehTorah, SeferHamitzvosLessons}
import org.opentorah.texts.tanach.{Custom, Haftarah, Reading, Span, Torah}
import org.opentorah.texts.tanach.Tanach.Psalms
import org.opentorah.util.Collections
import org.opentorah.xml.PrettyPrinter

import scala.xml.Elem

sealed abstract class Renderer(location: Location, spec: LanguageSpec) {
  protected type C <: Calendar[C]

  protected val calendar: C

  private final def getName(other: Boolean = false): String = if (!other) name else otherName

  protected def name: String

  protected def otherName: String

  private final def getYear(yearStr: String): C#Year = calendar.Year(yearStr.toInt)

  private final def getMonth(yearStr: String, monthStr: String): C#Month = {
    val year = getYear(yearStr)
    val monthName: Option[C#MonthName] = calendar.Month.Name.forName(monthStr)
    if (monthName.isDefined) year.month(monthName.get)
    else year.month(monthStr.toInt)
  }

  protected def jewish(day: DayBase[_]): Jewish.Day

  protected def gregorian(day: DayBase[_]): Gregorian.Day

  protected def first(day: DayBase[_]): DayBase[_]

  protected def second(day: DayBase[_]): DayBase[_]

  //  private def toLanguageString(what: LanguageString): String = what.toLanguageString(spec)

  private def yearUrl(year: YearBase[_], other: Boolean = false): String =
    s"/${getName(other)}/${year.number}"

  private def monthUrl(month: MonthBase[_], other: Boolean = false): String =
    s"/${getName(other)}/${month.year.number}/${month.numberInYear}"

  private def monthNameUrl(month: MonthBase[_], other: Boolean = false): String =
    s"/${getName(other)}/${month.year.number}/${month.name.toLanguageString(spec)}"

  private def dayUrl(day: DayBase[_], other: Boolean = false): String =
    s"/${getName(other)}/${day.year.number}/${day.month.numberInYear}/${day.numberInMonth}"

  private def yearLink(year: YearBase[_], other: Boolean = false, text: Option[String] = None): Elem =
    navLink(yearUrl(year, other = other), text.getOrElse(year.toLanguageString(spec)))

  private def monthLink(month: MonthBase[_]): Elem =
    navLink(monthUrl(month), month.numberInYearToLanguageString(spec))

  private def monthNameLink(month: MonthBase[_], other: Boolean = false, text: Option[String] = None): Elem =
    navLink(monthNameUrl(month, other = other), text.getOrElse(month.name.toLanguageString(spec)))

  private def dayLink(day: DayBase[_], other: Boolean = false, text: Option[String] = None): Elem =
    navLink(dayUrl(day, other = other), text.getOrElse(day.numberInMonthToLanguageString(spec)))

  private def navLink(url: String, text: String): Elem =
    <a class="nav" href={s"$url$suffix"}>{text}</a>

  private def suffix: String = Renderer.suffix(location, spec)

  private def dayLinks(day: DayBase[_], other: Boolean): Elem =
    <span>
    {yearLink(day.year, other = other)}
    {monthNameLink(day.month, other = other)}
    {if (day.number > 1) dayLink(day-1, other = other, text = Some("<")) else <span>{Renderer.earlyGregorianMessage} </span>}
    {dayLink(day, other = other)}
    {dayLink(day+1, other = other, text = Some(">"))}
    </span>

  def renderLanding: String = {
    val day: DayBase[_] = Calendars.now(calendar).day
    renderHtml(s"/$name", dayLinks(day, other = false))
  }

  def renderYear(yearStr: String): String = {
    val year: YearBase[_] = getYear(yearStr)
    renderHtml(
      yearUrl(year),
      <div>
        {yearLink(year-1, text = Some("<"))}
        {yearLink(year)}
        {yearLink(year+1, text = Some(">"))}
        <table>
          <tbody>
            {year.months.map { month: MonthBase[_] =>
            <tr>
              <td>{monthLink(month)}</td>
              <td>{monthNameLink(month)}</td>
            </tr>
          }}
          </tbody>
        </table>
        {renderYearInformation(year)}
      </div>
    )
  }

  protected def renderYearInformation(year: YearBase[_]): Seq[Elem] = Seq.empty

  def renderMonth(yearStr: String, monthStr: String): String = {
    val month: MonthBase[_] = getMonth(yearStr, monthStr)
    renderHtml(monthNameUrl(month),
      <div>
      {yearLink(month.year)}
      {monthNameLink(month-1, text = Some("<"))}
      {monthNameLink(month)}
      {monthNameLink(month+1, text = Some(">"))}
      <table>
        <tbody>
        {month.days.map { day: DayBase[_] => <tr><td>{dayLink(day)}</td></tr>}}
        </tbody>
      </table>
      </div>
    )
  }

  def renderDay(yearStr: String, monthStr: String, dayStr: String): String = {
    val day: DayBase[_] = getMonth(yearStr, monthStr).day(dayStr.toInt)
    val jewishDay: Jewish.Day = jewish(day)
    val firstDay: DayBase[_] = first(day)
    val secondDay: DayBase[_] = second(day)

    val daySchedule = Schedule.get(jewishDay, inHolyLand = location.inHolyLand)

    renderHtml(dayUrl(firstDay),
      <div>
      <div>{dayLinks(firstDay, other = false)} {firstDay.name.toLanguageString(spec)}</div>
      <div>{dayLinks(secondDay, other = true)} {secondDay.name.toLanguageString(spec)}</div>
      <div>{daySchedule.dayNames.map { withNames: WithNames =>
        <span class="name">{withNames.names.doFind(spec).name}</span>}}</div>
      {renderOptionalReading("Morning", daySchedule.morning)}
      {renderOptionalReading("Purim morning alternative", daySchedule.purimAlternativeMorning)}
      {if (!jewishDay.isShabbosMevarchim) Seq.empty[Elem] else renderShabbosMevarchim(jewishDay.month.next)}
      <span class="heading">Chitas</span>
      {renderChitas(daySchedule.chitas)}
      <span class="heading">Tehillim</span>
      {renderTehillim(jewishDay)}
      <div class="heading">Rambam</div>
      {renderRambam(RambamSchedule.forDay(jewishDay))}
      {renderOptionalReading("Afternoon", daySchedule.afternoon)}
      </div>
    )
  }

  private def renderShabbosMevarchim(month: Jewish.Month): Seq[Elem] = Seq(
    <span class="subheading">Shabbos Mevarchim</span>,
    <table><tr>
      <td>{month.name.toLanguageString(spec)}</td>
      {if (month.prev.length == 30) <td>{month.prev.lastDay.name.toLanguageString(spec)}</td> else <td/>}
      <td>{month.firstDay.name.toLanguageString(spec)}</td>
      <td>{month.newMoon.toLanguageString(spec)}</td>
    </tr></table>
  )

  private def renderChitas(chitas: Chitas): Elem = {
    def renderFragment(fragment: Torah.Fragment): Seq[Elem] = Seq(
      <td><span>{fragment.toLanguageString(spec)}</span></td>,
      <td>{renderSource(fragment.source)}</td>
    )

    <table><tbody>
      <tr>{renderFragment(chitas.first)}</tr> +:
      {chitas.second.fold(Seq.empty[Elem])(fragment => Seq(<tr>{renderFragment(fragment)}</tr>))}
    </tbody></table>
  }

  private def renderTehillim(day: Jewish.Day): Elem = {
    val forDayOfMonth: Span =
      if ((day.numberInMonth == 29) && day.month.length == 29) Span(Psalms.days(29-1).from, Psalms.days(30-1).to)
      else Psalms.days(day.numberInMonth-1)

    val forDayOfWeek: Span =
      Psalms.weekDays(day.numberInWeek-1)

    <table>
      <tr><td>for day of month</td><td><span>{forDayOfMonth.toLanguageString(spec)}</span></td></tr>
      <tr><td>for day of week</td><td><span>{forDayOfWeek.toLanguageString(spec)}</span></td></tr>
    </table>
  }

  private def renderRambam(schedule: RambamSchedule): Seq[Elem] = Seq(
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
    <table>{schedule.seferHamitzvos.parts.map { part: SeferHamitzvosLessons.Part =>
      <tr><td>{part.toLanguageString(spec)}</td></tr>
    }}</table>,
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

  private def renderRambamChapter(chapter: MishnehTorah.Chapter): Seq[Elem] = Seq(
    <td>{chapter.part.book.toLanguageString(spec)}</td>,
    <td>{chapter.part.toLanguageString(spec)}</td>,
    <td>{chapter.toLanguageString(spec)}</td>
  )

  private def renderOptionalReading(name: String, reading: Option[Reading]): Seq[Elem] = {
    reading.fold(Seq.empty[Elem]) { reading => Seq(<div>
      <span class="heading">{name}</span>
      {
        val maftirCommonOnly = reading.maftir.commonOnly
        val haftarahCommonOnly = reading.haftarah.commonOnly
        val noMaftirHaftarah: Boolean =
          maftirCommonOnly.fold(false)(_.isEmpty) && haftarahCommonOnly.fold(false)(_.isEmpty)
        val varyingMaftirAndHaftarah: Boolean = maftirCommonOnly.isEmpty && haftarahCommonOnly.isEmpty

        <div>
          {renderCustoms("Torah", reading.torah, renderTorah)}
          {if (noMaftirHaftarah) Seq.empty[Elem] else
          if (varyingMaftirAndHaftarah)
            renderCustoms("Maftir and Haftarah", reading.maftirAndHaftarah, renderMaftirAndHaftarah)
          else
            renderCustoms("Maftir", reading.maftir, renderMaftir) ++
            renderCustoms("Haftarah", reading.haftarah, renderHaftarah)
          }
        </div>
      }
    </div>)}
  }

  private def renderTorah(torah: Torah): Seq[Elem] =
    torah.spans.zipWithIndex map { case (fragment, index) =>
      <tr>
        <td>{spec.toString(index + 1)}</td>
        <td>{fragment.toLanguageString(spec)}</td>
        <td>{renderSource(fragment.source)}</td>
      </tr>
    }

  private def renderMaftir(maftir: Option[Torah.Maftir]): Seq[Elem] =
    Seq(maftir.fold(<tr><td>None</td></tr>)(maftir =>
      <tr>
        <td>{maftir.toLanguageString(spec)}</td>
        <td>{renderSource(maftir.source)}</td>
      </tr>
    ))

  private def renderHaftarah(haftarah: Option[Haftarah]): Seq[Elem] =
    haftarah.fold(Seq(<tr><td>None</td></tr>)){ haftarah =>
      val spans = haftarah.spans
      val parts: Seq[Seq[Haftarah.BookSpan]] =
        Collections.group[Haftarah.BookSpan, Option[WithNames]](spans, span => span.source)
      parts map { part: Seq[Haftarah.BookSpan] =>
        <tr>
          <td>{Haftarah.toLanguageString(part)(spec)}</td>
          <td>{renderSource(part.head.source)}</td>
        </tr>
      }}

  private def renderMaftirAndHaftarah(maftirAndHaftarah: Option[Reading.MaftirAndHaftarah]): Seq[Elem] =
    renderMaftir(maftirAndHaftarah.flatMap(_.maftir)) ++
      renderHaftarah(maftirAndHaftarah.map(_.haftarah))

  private def renderSource(source: Option[WithNames]): String =
    source.fold[String]("")(_.toLanguageString(spec))

  private def renderCustoms[T](
    what: String,
    customs: Custom.Of[T],
    renderer: T => Seq[Elem]
  ): Seq[Elem] =
    <span class="subheading">{what}</span> +:
    customs.customs.toSeq.map { case (custom: Custom, valueForCustom /*: T*/) =>
      <table class="custom">
        <caption>{custom.toLanguageString(spec)}</caption>
        <tbody>{renderer(valueForCustom)}</tbody>
      </table>
    }

  private def renderHtml(url: String, content: Elem): String =
    Renderer.renderHtml(url, content, location, spec)
}

object Renderer {
  private val jewishRendererName: String = "jewish"

  private val gregorianRenderername: String = "gregorian"

  private val earlyGregorianMessage: String = "Gregorian dates before year 1 are not supported!"

  private final class JewishRenderer(location: Location, spec: LanguageSpec) extends Renderer(location, spec) {
    override protected type C = Jewish

    override protected val calendar: C = Jewish

    override protected def name: String = jewishRendererName

    override protected def otherName: String = gregorianRenderername

    override protected def jewish(day: DayBase[_]): Jewish.Day = day.asInstanceOf[Jewish.Day]

    override protected def gregorian(day: DayBase[_]): Gregorian.Day = {
      try {
        Calendars.fromJewish(jewish(day))
      } catch {
        case _: IllegalArgumentException => Gregorian.Year(1).month(1).day(1)
      }
    }

    override protected def first(day: DayBase[_]): DayBase[_] = jewish(day)

    override protected def second(day: DayBase[_]): DayBase[_] = gregorian(day)

    override protected def renderYearInformation(yearRaw: YearBase[_]): Seq[Elem] = {
      val year: Jewish.Year = yearRaw.asInstanceOf[Jewish.Year]
      val delay = year.newYearDelay

      val numbers: Elem =
        <table>
          <tr><td>from creation</td><td>{year.toLanguageString(spec)}</td></tr>
          <tr><td>is leap</td><td>{year.isLeap.toString}</td></tr>
          <tr><td>months</td><td>{spec.toString(year.lengthInMonths)}</td></tr>
          <tr><td>days</td><td>{spec.toString(year.lengthInDays)}</td></tr>
          <tr><td>type</td><td>{YearType.forYear(year).toString}</td></tr>
          <tr><td>Molad</td><td>{year.newMoon.toLanguageString(spec)}</td></tr>
          <tr><td>New Year Delay</td><td>{s"$delay (${delay.days})"}</td></tr>
        </table>

      def cycle(name: String, yearsCycle: YearsCycle): Elem = {
        val in = yearsCycle.forYear(year)
        <tr>
          <td>{name}</td>
          <td>{spec.toString(in.cycleNumber)}</td>
          <td>{spec.toString(in.numberInCycle)}</td>
        </tr>
      }

      val cycles: Elem =
        <table>
          <tr><td>Cycle"</td><td>Number</td><td>In Cycle"</td></tr>
          {cycle("Leap Years", LeapYearsCycle)}
          {cycle("Shemittah", Shemittah)}
          {cycle("Birchas Hachamo", Sun.Shmuel)}
        </table>

      val tkufot: Elem = {
        def tkufa(flavor: Season.ForYear, season: Season): String =
          flavor.seasonForYear(season, year).toLanguageString(spec)

        <table>
          <tr><td>Tkufa</td><td>Shmuel</td><td>Rav Ada"</td></tr>
          {Season.values.map { season =>
          <tr>
            <td>{season.toLanguageString(spec)}</td>
            <td>{tkufa(Sun.Shmuel, season)}</td>
            <td>{tkufa(Sun.RavAda, season)}</td>
          </tr>}}
        </table>
      }

      val festivalDays: Seq[(SpecialDay, Jewish.Day)] =
        SpecialDay.daysWithSpecialReadings(location == Location.HolyLand)
          .map(specialDay => specialDay -> specialDay.correctedDate(year))
          .toSeq.sortBy(_._2)

      val festivals: Elem =
        <table>
        {festivalDays.map { case (specialDay, day) =>
        <tr>
          <td>{specialDay.toLanguageString(spec)}</td>
          <td>{day.toLanguageString(spec)}</td>
          <td>{day.name.toLanguageString(spec)}</td>
        </tr>}}
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
    }
  }

  private final class GregorianRenderer(location: Location, spec: LanguageSpec) extends Renderer(location, spec) {
    override protected type C = Gregorian

    override protected val calendar: C = Gregorian

    override protected def name: String = gregorianRenderername

    override protected def otherName: String = jewishRendererName

    override protected def jewish(day: DayBase[_]): Jewish.Day = Calendars.toJewish(gregorian(day))

    override protected def gregorian(day: DayBase[_]): Gregorian.Day = day.asInstanceOf[Gregorian.Day]

    override protected def first(day: DayBase[_]): DayBase[_] = gregorian(day)

    override protected def second(day: DayBase[_]): DayBase[_] = jewish(day)
  }

  def renderer(kindStr: String, location: Location, spec: LanguageSpec): Renderer = {
    if (kindStr == jewishRendererName) new JewishRenderer(location, spec)
    else if (kindStr == gregorianRenderername) new GregorianRenderer(location, spec)
    else throw new IllegalArgumentException(s"Unrecognized kind $kindStr")
  }

  def renderRoot(location: Location, spec: LanguageSpec): String = renderHtml(
    url = "/",
    content =
      <div>
        <div><a href={s"/$jewishRendererName"}>jewish"</a></div>,
        <div><a href={s"/$gregorianRenderername"}>gregorian</a></div>
      </div>,
    location = location,
    spec = spec
  )

  def renderHtml(
    url: String,
    content: Elem,
    location: Location,
    spec: LanguageSpec
  ): String = {
    val languages: Seq[Elem] = Language.values.map(_.toSpec) map { spec1 =>
      val languageName = spec1.languageName
      if (spec1.language == spec.language) <span class="picker">{languageName}</span>
      else <a class="picker" href={s"$url${suffix(location, spec1)}"}>{languageName}</a>
    }

    val locations: Seq[Elem] = Seq(Location.HolyLand, Location.Diaspora).map { location1 =>
      if (location1 == location) <span class="picker">{location1.name}</span>
      else <a class="picker" href={s"$url${suffix(location1, spec)}"}>{location1.name}</a>
    }

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

    PrettyPrinter.default.render(result)
  }

  private def direction(spec: LanguageSpec): String =
    if (spec.language.contains(Language.Hebrew)) "rtl" else "ltr"

  private def suffix(location: Location, spec: LanguageSpec): String =
    s"?inHolyLand=${location.inHolyLand}&lang=${spec.languageName}"
}
