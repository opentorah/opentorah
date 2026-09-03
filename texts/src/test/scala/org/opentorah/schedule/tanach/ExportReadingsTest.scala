package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.Jewish.{Day, Year}
import org.opentorah.calendar.roman.Gregorian
import org.opentorah.texts.tanach.{Custom, Haftarah, Reading, Torah, WeeklyReading}
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.file.{Files, Paths}
import scala.collection.mutable

/** Writes the readings of every situation to a TSV, for the Java port to check
  * itself against. Not a test of anything; it is here because this is where the
  * schedule and its test classpath already are. Enable it with -Dexport=<path>.
  */
final class ExportReadingsTest extends AnyFlatSpec:

  private val FromYear = 5780
  private val ToYear = 5860

  "readings" should "be exported" in:
    val target = System.getenv("EXPORT_READINGS")
    if target == null then cancel("set EXPORT_READINGS=<path> to write the fixture")

    val customs: Seq[Custom] = Custom.values.toSeq.filter(_ != Custom.Common)

    // situation -> (first date it occurred, its lines)
    val seen: mutable.LinkedHashMap[String, (String, Seq[String])] = mutable.LinkedHashMap()

    for
      yearNumber <- FromYear to ToYear
      inHolyLand <- Seq(false, true)
    do
      val schedule = Schedule(Year(yearNumber), inHolyLand)
      // the next year too: a day late in Elul takes its parsha from a Shabbos
      // that already belongs to the year after it
      val weekly: Map[Day, WeeklyReading] =
        Schedule.weeklyReadingsForYear(Year(yearNumber), inHolyLand) ++
        Schedule.weeklyReadingsForYear(Year(yearNumber + 1), inHolyLand)
      for day <- schedule.days.keys.toSeq.sortBy(_.number) do
        val daySchedule = schedule.days(day)
        val date = gregorian(day)
        // A weekday reads from the parsha of the Shabbos it is heading towards,
        // so that is the one that belongs in its situation: without it every
        // ordinary Monday is the same situation and only one gets recorded.
        val parshaOfWeek: Option[WeeklyReading] =
          weekly.get(if day.isShabbos then day else day.shabbosAfter)
        val prefix = situationPrefix(day, daySchedule, parshaOfWeek, inHolyLand)
        for (slot, reading) <- Seq("morning" -> daySchedule.morning, "afternoon" -> daySchedule.afternoon) do
          reading.foreach(reading =>
            val situation = s"$prefix|$slot"
            if !seen.contains(situation) then
              seen(situation) = (date, lines(situation, reading, customs))
          )

    val out = seen.values.toSeq
      .flatMap((date, ls) => ls.map(line => s"$date\t$line"))
      .sorted
    Files.write(Paths.get(target), (header ++ out).mkString("", "\n", "\n").getBytes("UTF-8"))
    println(s"wrote ${out.size} lines for ${seen.size} situations to $target")

  private def header: Seq[String] = Seq(
    "# Expected readings, exported from opentorah — the oracle for the Java port.",
    "#",
    s"# opentorah ${sys.env.getOrElse("EXPORT_COMMIT", "(working tree)")}  (Apache 2.0; see opentorah/LICENSE.md)",
    s"# generated ${java.time.LocalDate.now} over Jewish years $FromYear-$ToYear, both locations",
    "#",
    "# date<TAB>situation<TAB>customs<TAB>kind<TAB>value",
    "#",
    "# date is Gregorian yyyy-mm-dd — deliberately, so no consumer has to",
    "# translate month numbering: opentorah counts Jewish months from Tishrei",
    "# and this library counts them from Nisan. It is the first date on which",
    "# the situation occurs. The situation names everything that determines",
    "# the reading — location, day of week, the week's parsha, and what else",
    "# applies — because readings are a function of the combination, and",
    "# combinations are where the bugs are. Customs are those reading the same",
    "# thing; ALL is every custom. Spans are book:fromCh:fromV:toCh:toV, torah",
    "# aliyot prefixed by number, '-' means nothing is read."
  )

  private def gregorian(day: Day): String =
    val g = day.to(Gregorian)
    f"${g.year.number}%04d-${g.month.numberInYear}%02d-${g.numberInMonth}%02d"

  private def situationPrefix(
    day: Day,
    daySchedule: Schedule.DaySchedule,
    weekly: Option[WeeklyReading],
    inHolyLand: Boolean
  ): String =
    val location = if inHolyLand then "EY" else "chul"
    val dayOfWeek =
      if day.isShabbos then "shabbos"
      else java.time.LocalDate.parse(gregorian(day)).getDayOfWeek.toString.toLowerCase.capitalize
    val parsha = weekly.fold("-")(weekly =>
      weekly.parsha.toString + weekly.secondParsha.fold("")(second => "-" + second.toString))
    // sorted, so that a situation has one name however it was assembled
    val names = daySchedule.dayNames.map(_.toString).sorted
    s"$location|$dayOfWeek|$parsha|${if names.isEmpty then "-" else names.mkString("+")}"

  /** One line per (kind, group of customs reading the same thing). */
  private def lines(situation: String, reading: Reading, customs: Seq[Custom]): Seq[String] =
    Seq(
      "torah" -> ((custom: Custom) => renderTorah(reading.doFind(custom).torah)),
      "maftir" -> ((custom: Custom) => renderMaftir(reading.doFind(custom).maftirAndHaftarah.flatMap(_.maftir))),
      "haftarah" -> ((custom: Custom) => renderHaftarah(reading.doFind(custom).maftirAndHaftarah.map(_.haftarah)))
    ).flatMap((kind, render) =>
      val byValue: Seq[(String, Seq[Custom])] =
        customs.groupBy(render).toSeq.map((value, cs) => (value, cs.sortBy(_.toString)))
      byValue.sortBy((_, cs) => cs.head.toString).map((value, cs) =>
        val who = if cs.size == customs.size then "ALL" else cs.map(_.toString).mkString("+")
        s"$situation\t$who\t$kind\t$value"
      )
    )

  private def renderTorah(torah: Torah): String =
    if torah.spans.isEmpty then "-"
    else torah.spans.zipWithIndex.map((span, i) => s"${i + 1}:${renderSpan(span.book.toString, span.span)}").mkString(" ")

  private def renderMaftir(maftir: Option[Torah.Maftir]): String =
    maftir.fold("-")(m => renderSpan(m.book.toString, m.span))

  private def renderHaftarah(haftarah: Option[Haftarah]): String =
    haftarah.fold("-")(h => h.spans.map(s => renderSpan(s.book.toString, s.span)).mkString(" "))

  private def renderSpan(book: String, span: org.opentorah.texts.tanach.Span): String =
    s"${book.replace(" ", "")}:${span.from.chapter}:${span.from.verse}:${span.to.chapter}:${span.to.verse}"
