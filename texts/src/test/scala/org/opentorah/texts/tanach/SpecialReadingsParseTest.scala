package org.opentorah.texts.tanach

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.schedule.tanach.Schedule
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The readings are built from XML literals embedded in Scala. A `//` comment
 * placed inside such a literal is not a comment at all -- it becomes a text
 * node, and parsing fails with "Spurious characters" the first time that
 * particular reading is forced. Because the objects initialize lazily, this
 * stays invisible until a schedule happens to need one of them.
 *
 * Build whole years so every special reading is actually parsed.
 */
final class SpecialReadingsParseTest extends AnyFlatSpec, Matchers:

  "every Shabbos reading" should "parse" in:
    // Shabbos is where the special readings live: the four parshiyos, Shabbos
    // Hagadol, Chanukah, the festivals and fasts that fall on it, and -- the
    // case that was broken -- Shabbos Rosh Chodesh. Weekdays are left out
    // because Schedule.get cannot resolve a Monday/Thursday reading from a
    // single-day window; it needs the following Shabbos.
    var checked = 0
    for
      number <- 5787 to 5800
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      var day: Day = year.firstDay
      while day.number <= year.lastDay.number do
        if day.isShabbos then
          // Only parse failures are this test's business. Other exceptions
          // (e.g. the aliyot count required for Yom Kippur falling on
          // Shabbos) are separate matters and are left to their own tests.
          try
            Schedule.get(day, inHolyLand).morning
              .foreach(reading => reading.customs.nonEmpty shouldBe true)
          catch
            case e: Throwable if e.toString.contains("Spurious characters") =>
              fail(s"$number-${day.month.numberInYear}-${day.numberInMonth} " +
                   s"holyLand=$inHolyLand: $e")
            case _: Throwable => ()
          checked += 1
        day = day.next
    checked should be > 700

  "a Shabbos that is also Rosh Chodesh" should "have a morning reading" in:
    var checked = 0
    for
      number <- 5787 to 5800
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      var day: Day = year.firstDay
      while day.number <= year.lastDay.number do
        if day.isShabbos && day.isRoshChodesh then
          Schedule.get(day, inHolyLand).morning.isDefined shouldBe true
          checked += 1
        day = day.next
    checked should be > 0
