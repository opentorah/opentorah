package org.opentorah.schedule.tanach

import org.opentorah.calendar.jewish.{Jewish, NewYear}
import org.opentorah.calendar.jewish.SpecialDay.*
import Jewish.{Day, Year}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A public fast is read with Vayechal in the morning as well as at Mincha.
 * getWeekdayMorningReading had no case for the fasts, so it fell through to
 * its throw and no fast on a weekday could be scheduled at all -- which also
 * meant a whole year could not be built, since every year contains fasts.
 */
final class PublicFastTest extends AnyFlatSpec, Matchers:

  private val fasts = Seq(FastOfGedalia, FastOfTeves, FastOfEster, FastOfTammuz, TishaBeAv)

  "a public fast on a weekday" should "have a morning and an afternoon reading" in:
    var weekdays = 0
    var moved = 0
    for
      number <- 5700 to 5900
      inHolyLand <- Seq(false, true)
      fast <- fasts
    do
      val day = fast.date(Year(number))
      val schedule = Schedule.get(day, inHolyLand)
      withClue(s"$number ${fast.getClass.getSimpleName} shabbos=${day.isShabbos}: ") {
        schedule.morning.isDefined shouldBe true
        // date() is the nominal date; a fast landing on Shabbos is moved, and
        // that Shabbos is an ordinary Shabbos with no fast reading.
        if day.isShabbos then moved += 1 else
          weekdays += 1
          schedule.afternoon.isDefined shouldBe true
      }
    weekdays should be > 0
    moved should be > 0

  it should "read three aliyot" in:
    for
      number <- 5700 to 5900
      fast <- fasts
    do
      val day = fast.date(Year(number))
      if !day.isShabbos then
        withClue(s"$number ${fast.getClass.getSimpleName}: ")(
          Schedule.get(day, inHolyLand = false).morning.get
            .torah.customs.values.map(_.length).toSet shouldBe Set(3))

  "a whole year" should "be buildable" in:
    // What #149 asks for: retrieve every day's schedule for a year. Before the
    // fasts were wired in this threw partway through every single year.
    for
      number <- Seq(5787, 5788, 5789, 5790)
      inHolyLand <- Seq(false, true)
    do
      val year = Year(number)
      val schedule = Schedule(year, inHolyLand)
      var day: Day = year.firstDay
      var days = 0
      while day.number <= year.lastDay.number do
        schedule.days.contains(day) shouldBe true
        days += 1
        day = day.next
      days should be > 350
