package org.opentorah.calendar.service

import org.opentorah.calendar.{Calendar, YearsCycle}
import org.opentorah.calendar.jewish.{Jewish, LeapYearsCycle, NewYear, Season, Shemittah, SpecialDay, Sun, YearType}
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.opentorah.metadata.{Language, Named}
import org.opentorah.schedule.rambam.RambamSchedule
import org.opentorah.schedule.tanach.{Chitas, Schedule}
import org.opentorah.texts.rambam.{MishnehTorah, SeferHamitzvosLessons}
import org.opentorah.texts.tanach.Tanach.Psalms
import org.opentorah.texts.tanach.{Custom, Haftarah, Reading, Span, Torah}
import org.opentorah.util.Collections
import org.opentorah.xml.{A, PrettyPrinter, Xml}

final class Renderer(
  val calendar: Calendar,
  location: Location,
  spec: Language.Spec
):

  private given Language.Spec = spec

  // TODO move into CalendarService
  private def getYear(yearStr: String): calendar.Year = calendar.Year(yearStr.toInt)

  // TODO move into CalendarService
  private def getMonth(yearStr: String, monthStr: String): calendar.Month =
    val year = getYear(yearStr)
    val monthName: Option[calendar.Month.Name] = calendar.Month.forName(monthStr)
    if monthName.isDefined then year.month(monthName.get)
    else year.month(monthStr.toInt)

  def renderLanding: String = renderHtml(
    url(),
    dayLinks(Gregorian.fromLocalDateTime(java.time.LocalDateTime.now()).to(calendar).day, calendar)
  )

  // TODO move into CalendarService
  def renderYear(yearStr: String): String = renderYear(getYear(yearStr))

  def renderYear(year: calendar.Year): String =
    val yearInformation: Seq[Xml.Element] = calendar match
      case _: Jewish.type => renderJewishYearInformation(year.asInstanceOf[Jewish.Year]) // TODO cast
      case _ => Seq.empty

    renderHtml(
      yearUrl(year),
      <div>
      {yearLink(year.prev, text = Some("<"))}
      {yearLink(year)}
      {yearLink(year.next, text = Some(">"))}
      <table><tbody>
      {year.months.map((month: Calendar#Month) => <tr><td>{monthLink(month)}</td> <td>{monthNameLink(month)}</td></tr>)}
      </tbody></table>
      {yearInformation}
      </div>
    )

  def renderMonth(yearStr: String, monthStr: String): String =
    val month: calendar.Month = getMonth(yearStr, monthStr)
    renderHtml(monthNameUrl(month),
      <div>
      {yearLink(month.year)}
      {monthNameLink(month.prev, text = Some("<"))}
      {monthNameLink(month)}
      {monthNameLink(month.next, text = Some(">"))}
      <table><tbody>
        {month.days.map((day: Calendar#Day) => <tr><td>{dayLink(day)}</td></tr>)}
      </tbody></table>
      </div>
    )

  private def dayLinks(day: calendar.Day, calendarTo: Calendar): Xml.Element =
    val rendererTo: Renderer = Renderer(
      calendarTo,
      location,
      spec
    )
    rendererTo.dayLinks(day.to(calendarTo).asInstanceOf[rendererTo.calendar.Day]) // TODO cast...

  private def dayLinks(day: calendar.Day): Xml.Element =
    <div>
      <span>
        {calendar.name}
        {yearLink(day.year)}
        {monthNameLink(day.month)}
        {if day.number > 1 then dayLink(day.prev, text = Some("<")) else <span>{Renderer.earlyGregorianMessage} </span>}
        {dayLink(day)}
        {dayLink(day.next, text = Some(">"))}
        {day.name.toLanguageString}
      </span>
    </div>

  // TODO move into CalendarService
  def renderDay(yearStr: String, monthStr: String, dayStr: String): String =
    renderDay(getMonth(yearStr, monthStr).day(dayStr.toInt))

  def renderDay(day: calendar.Day): String =
    val jewishDay: Jewish.Day = calendar match
      case _: Jewish.type => day.asInstanceOf[Jewish.Day] // TODO cast...
      case _ => day.to(Jewish)

    val calendars: Seq[Calendar] = calendar +: Renderer.calendars.filterNot(_.name == calendar.name)

    val daySchedule = Schedule.get(jewishDay, inHolyLand = location.inHolyLand)

    renderHtml(dayUrl(day),
      <div>
        {for calendar <- calendars yield dayLinks(day, calendar)}
        <div>{daySchedule.dayNames.map((named: Named) => <span class="name">{named.names.doFind(spec).name}</span>)}</div>
        {renderOptionalReading("Morning", daySchedule.morning)}
        {renderOptionalReading("Purim morning alternative", daySchedule.purimAlternativeMorning)}
        {if !jewishDay.isShabbosMevarchim then Seq.empty[Xml.Element] else renderShabbosMevarchim(jewishDay.month.next)}
        <span class="heading">Chitas</span>
        {renderChitas(daySchedule.chitas)}
        <span class="heading">Tehillim</span>
        {renderTehillim(jewishDay)}
        <div class="heading">Rambam</div>
        {renderRambam(RambamSchedule.forDay(jewishDay))}
        {renderOptionalReading("Afternoon", daySchedule.afternoon)}
      </div>
    )

  private def renderShabbosMevarchim(month: Jewish.Month): Seq[Xml.Element] = Seq(
    <span class="subheading">Shabbos Mevarchim</span>,
    <table><tr>
      <td>{month.name.toLanguageString}</td>
      {if month.prev.length == 30 then <td> {month.prev.lastDay.name.toLanguageString} </td> else <td/>}
      <td>{month.firstDay.name.toLanguageString}</td>
      <td>{month.newMoon.toLanguageString}</td>
    </tr></table>
  )

  private def renderChitas(chitas: Chitas): Xml.Element =
    def renderFragment(fragment: Torah.Fragment): Seq[Xml.Element] = Seq(
      <td><span>{fragment.toLanguageString}</span></td>,
      <td>{renderSource(fragment.source)}</td>
    )

    <table><tbody>
      <tr>{renderFragment(chitas.first)}</tr>
      {chitas.second.fold(Seq.empty[Xml.Element])(fragment => Seq(<tr>{renderFragment(fragment)}</tr>))}
    </tbody></table>

  private def renderTehillim(day: Jewish.Day): Xml.Element =
    val forDayOfMonth: Span =
      if (day.numberInMonth == 29) && day.month.length == 29 then Span(Psalms.days(29 - 1).from, Psalms.days(30 - 1).to)
      else Psalms.days(day.numberInMonth - 1)

    val forDayOfWeek: Span =
      Psalms.weekDays(day.numberInWeek - 1)

    <table>
      <tr><td>for day of month</td> <td><span>{forDayOfMonth.toLanguageString}</span></td></tr>
      <tr><td>for day of week</td> <td><span>{forDayOfWeek.toLanguageString}</span></td></tr>
    </table>

  private def renderRambam(schedule: RambamSchedule): Seq[Xml.Element] = Seq(
    <span class="subheading">3 chapters</span>,
    <table>
      <tr><td>Cycle</td> <td>Lesson</td></tr>
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
    )}
    </table>,
    <span class="subheading">1 chapter</span>,
    <table>
      <tr><td>Cycle</td> <td>Year</td> <td>Chapter number</td></tr>
      <tr>
        <td>{spec.toString(schedule.oneChapter.cycle)}</td>
        <td>{spec.toString(schedule.oneChapter.year)}</td>
        <td>{spec.toString(schedule.oneChapter.chapterNumber)}</td>
      </tr>
    </table>,
    <span class="subheading">chapter</span>,
    <table>
      <tr><td>Book</td> <td>Part</td> <td>Chapter</td></tr>
      <tr>{renderRambamChapter(schedule.oneChapter.chapter)}</tr>
    </table>
  )

  private def renderRambamChapter(chapter: MishnehTorah.Chapter): Seq[Xml.Element] = Seq(
    <td>{chapter.part.book.toLanguageString}</td>,
    <td>{chapter.part.toLanguageString}</td>,
    <td>{chapter.toLanguageString}</td>
  )

  private def renderOptionalReading(name: String, reading: Option[Reading]): Seq[Xml.Element] =
    reading.fold(Seq.empty[Xml.Element])(reading => Seq(<div>
      <span class="heading">{name}</span>
      {
        val maftirCommonOnly = reading.maftir.commonOnly
        val haftarahCommonOnly = reading.haftarah.commonOnly
        val noMaftirHaftarah: Boolean =
          maftirCommonOnly.fold(false)(_.isEmpty) && haftarahCommonOnly.fold(false)(_.isEmpty)
        val varyingMaftirAndHaftarah: Boolean = maftirCommonOnly.isEmpty && haftarahCommonOnly.isEmpty

        <div>
          {renderCustoms("Torah", reading.torah, renderTorah)}
          {if noMaftirHaftarah then Seq.empty[Xml.Element] else
            if varyingMaftirAndHaftarah then
              renderCustoms("Maftir and Haftarah", reading.maftirAndHaftarah, renderMaftirAndHaftarah)
            else
              renderCustoms("Maftir", reading.maftir, renderMaftir) ++
              renderCustoms("Haftarah", reading.haftarah, renderHaftarah)
          }
        </div>
      }
    </div>))

  private def renderTorah(torah: Torah): Seq[Xml.Element] =
    torah.spans.zipWithIndex map ((fragment, index) =>
      <tr>
        <td>{spec.toString(index + 1)}</td>
        <td>{fragment.toLanguageString}</td>
        <td>{renderSource(fragment.source)}</td>
      </tr>
    )

  private def renderMaftir(maftir: Option[Torah.Maftir]): Seq[Xml.Element] =
    Seq(maftir.fold(<tr><td>None</td></tr>)(maftir =>
      <tr>
        <td>{maftir.toLanguageString}</td>
        <td>{renderSource(maftir.source)}</td>
      </tr>
    ))

  private def renderHaftarah(haftarah: Option[Haftarah]): Seq[Xml.Element] =
    haftarah.fold(Seq(<tr>
      <td>None</td>
    </tr>))(haftarah =>
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

  private def renderMaftirAndHaftarah(maftirAndHaftarah: Option[Reading.MaftirAndHaftarah]): Seq[Xml.Element] =
    renderMaftir(maftirAndHaftarah.flatMap(_.maftir)) ++
    renderHaftarah(maftirAndHaftarah.map(_.haftarah))

  private def renderSource(source: Option[Named]): String =
    source.fold[String]("")(_.toLanguageString)

  private def renderCustoms[T](
    what: String,
    customs: Custom.Of[T],
    renderer: T => Seq[Xml.Element]
  ): Seq[Xml.Element] =
    <span class="subheading">{what}</span> +:
    customs.customs.toSeq.map((custom: Custom, valueForCustom /*: T*/) =>
      <table class="custom">
        <caption>{custom.toLanguageString}</caption>
        <tbody>{renderer(valueForCustom)}</tbody>
      </table>
    )

  private def yearLink(year: Calendar#Year, text: Option[String] = None): Xml.Element =
    navLink(yearUrl(year), text.getOrElse(year.toLanguageString))

  private def yearUrl(year: Calendar#Year): Seq[String] =
    url(year.number.toString)

  private def monthUrl(month: Calendar#Month): Seq[String] =
    url(month.year.number.toString, month.numberInYear.toString)

  private def monthLink(month: Calendar#Month): Xml.Element =
    navLink(monthUrl(month), month.numberInYearToLanguageString)

  private def monthNameLink(month: Calendar#Month, text: Option[String] = None): Xml.Element =
    navLink(monthNameUrl(month), text.getOrElse(month.name.toLanguageString))

  private def monthNameUrl(month: Calendar#Month): Seq[String] =
    url(month.year.number.toString, month.name.toLanguageString)

  private def dayLink(day: Calendar#Day, text: Option[String] = None): Xml.Element =
    navLink(dayUrl(day), text.getOrElse(day.numberInMonthToLanguageString))

  private def dayUrl(day: Calendar#Day): Seq[String] =
    url(day.year.number.toString, day.month.numberInYear.toString, day.numberInMonth.toString)

  private def url(parts: String*): Seq[String] = calendar.name +: parts

  private def navLink(url: Seq[String], text: String): Xml.Element =
    A(url).setQuery(Renderer.suffix(location, spec)).addClass("nav")(text)

  private def renderHtml(url: Seq[String], content: Xml.Element): String =
    Renderer.renderHtml(url, content, location, spec)

  private def renderJewishYearInformation(year: Jewish.Year): Seq[Xml.Element] =
    val delay: NewYear.Delay = year.newYearDelay

    val numbers: Xml.Element =
      <table>
        <tr><td>from creation</td> <td>{year.toLanguageString}</td></tr>
        <tr><td>is leap</td> <td>{year.isLeap.toString}</td></tr>
        <tr><td>months</td> <td>{spec.toString(year.lengthInMonths)}</td></tr>
        <tr><td>days</td> <td>{spec.toString(year.lengthInDays)}</td></tr>
        <tr><td>type</td> <td>{YearType.forYear(year).toString}</td></tr>
        <tr><td>Molad</td> <td>{year.newMoon.toLanguageString}</td></tr>
        <tr><td>New Year Delay</td> <td>{s"$delay (${delay.days})"}</td></tr>
      </table>

    def cycle(name: String, yearsCycle: YearsCycle): Xml.Element =
      val in = yearsCycle.forYear(Jewish)(year)
      <tr>
        <td>{name}</td>
        <td>{spec.toString(in.cycleNumber)}</td>
        <td>{spec.toString(in.numberInCycle)}</td>
      </tr>

    val cycles: Xml.Element =
      <table>
        <tr><td>Cycle"</td> <td>Number</td> <td>In Cycle"</td></tr>
        {cycle("Leap Years", LeapYearsCycle)}
        {cycle("Shemittah", Shemittah)}
        {cycle("Birchas Hachamo", Sun.Shmuel)}
      </table>

    val tkufot: Xml.Element =
      def tkufa(flavor: Season.ForYear, season: Season): String =
        flavor.seasonForYear(season, year).toLanguageString

      <table>
        <tr><td>Tkufa</td> <td>Shmuel</td> <td>Rav Ada</td></tr>
        {Season.valuesSeq.map { season =>
          <tr>
            <td>{season.toLanguageString}</td>
            <td>{tkufa(Sun.Shmuel, season)}</td>
            <td>{tkufa(Sun.RavAda, season)}</td>
          </tr>
        }}
      </table>

    val festivalDays: Seq[(SpecialDay, Jewish.Day)] =
      SpecialDay.daysWithSpecialReadings(location == Location.HolyLand)
        .map(specialDay => specialDay -> specialDay.correctedDate(year))
        .toSeq.sortBy(_._2)

    val festivals: Xml.Element =
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

object Renderer:
  val calendars: Seq[Calendar] = Seq(Jewish, Gregorian, Julian)

  private val earlyGregorianMessage: String = "Gregorian dates before year 1 are not supported!"

  def renderRoot(location: Location, spec: Language.Spec): String = renderHtml(
    url = Seq.empty,
    content = <div>{for calendar <- calendars yield <div>{A(Seq(calendar.name))(text = calendar.name)}</div>}</div>,
    location = location,
    spec = spec
  )

  def renderHtml(
    url: Seq[String],
    content: Xml.Element,
    location: Location,
    spec: Language.Spec
  ): String =
    val languages: Seq[Xml.Element] = Language.valuesSeq.map(_.toSpec).map(spec1 =>
      val languageName = spec1.languageName
      if spec1.language == spec.language then <span class="picker">{languageName}</span>
      else A(url).setQuery(suffix(location, spec1)).addClass("picker")(text = languageName)
    )

    val locations: Seq[Xml.Element] = Seq(Location.HolyLand, Location.Diaspora).map(location1 =>
      if location1 == location then <span class="picker">{location1.name}</span>
      else A(url).setQuery(suffix(location1, spec)).addClass("picker")(text = location1.name)
    )

    //        title("Reading Schedule")?
    val result =
      <html dir={direction(spec)}>
        <head><link rel="stylesheet" type="text/css" href="/style.css"/></head>
        <body>{languages}{locations}{content}</body>
      </html>

    PrettyPrinter.default.render(element = result)

  private def direction(spec: Language.Spec): String =
    if spec.language.contains(Language.Hebrew) then "rtl" else "ltr" // TODO add a method to Language...

  private def suffix(location: Location, spec: Language.Spec): String =
    s"${Location.queryParameterName}=${location.inHolyLand}&lang=${spec.languageName}"
