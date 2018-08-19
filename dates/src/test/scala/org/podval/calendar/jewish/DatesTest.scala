package org.podval.calendar.jewish

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import Jewish.{Day, Month, Year}
import Jewish.TimeVector
import Jewish.Day.Name._
import Jewish.Month.Name._
import SpecialDay._
import org.podval.calendar.dates.Calendar
import org.podval.calendar.gregorian.Gregorian
import Gregorian.Month.Name._


final class DatesTest extends FlatSpec with Matchers {

  "Known dates" should "have correct day of the week" in {
    Day(5772, Marheshvan, 24).name shouldBe Day.Name.Sheni
  }

  "Conversions from date to days and back" should "end where they started" in {
    val data = Table(
      ("yearNumber", "monthName", "dayNumber"),
      (1   , Tishrei,  1),
      (2   , Tishrei,  1),
      (5768, AdarII , 28),
      (5769, Nisan  , 14)
    )

    forAll(data) {
      (
        yearNumber: Int,
        monthName: Month.Name,
        dayNumber: Int
      ) =>
        val year = Year(yearNumber)
        year.number shouldBe yearNumber

        val month = year.month(monthName)
        month.name shouldBe monthName

        val day = month.day(dayNumber)
        day.year shouldBe year
        day.month shouldBe month
        day.numberInMonth shouldBe dayNumber
    }
  }

  "New moons from the printed tables" should "calculate correctly" in {
    val data = Table(
      // year and month for the molad; jewish date; georgian date; georgian time
      ("moladYear", "moladMonth", "dayOfWeek", "year", "month", "day", "yearG", "monthG", "dayG", "hours", "minutes", "parts"),

      (5769, Tishrei   , Shlishi ,  5769, Tishrei,  1,  2008, September, 30,  1, 58, 13),
      (5769, Marheshvan, Rvii    ,  5769, Tishrei, 30,  2008, October  , 29, 14, 42, 14),
      (5769, Kislev    , Shishi  ,  5769, Kislev ,  1,  2008, November , 28,  3, 26, 15),

      (5771, Tishrei   , Chamishi,  5771, Tishrei,  1,  2010, September,  8, 19, 36,  1),

      (5772, Tishrei   , Shlishi ,  5771, Elul   , 28,  2011, September, 27, 17,  8, 14),
      (5772, Marheshvan, Chamishi,  5772, Tishrei, 29,  2011, October  , 27,  5, 52, 15),

      (5773, Tishrei   , Rishon  ,  5772, Elul   , 29,  2012, September, 16,  1, 57,  8),
      (5773, Adar      , Rishon  ,  5773, Shvat  , 30,  2013, February , 10, 17, 37, 13),
      (5773, Nisan     , Shlishi ,  5773, Nisan  ,  1,  2013, March    , 12,  6, 21, 14)
    )

    forAll(data) {
      (
        moladYear: Int, moladMonth: Jewish.MonthName, dayOfWeek: Jewish.DayName,
        year: Int, month: Jewish.MonthName, day: Int,
        yearG: Int, monthG: Gregorian.MonthName, dayG: Int,
        hours: Int, minutes: Int, parts: Int
      ) =>
        val dayJ = Jewish.Day(year, month, day)
        val dateG = Gregorian.Day(yearG, monthG, dayG).toMoment.
          hours(hours).minutes(minutes).partsWithoutMinutes(parts)
        val molad = Jewish.Year(moladYear).month(moladMonth).newMoon

        molad.day.name shouldBe dayOfWeek
        molad.day shouldBe dayJ

        Calendar.toJewish(dateG).day shouldBe dayJ
        Calendar.fromJewish(molad) shouldBe dateG
    }
  }

  "Moment components" should "be correct" in {
    val data = Table(
      ("days", "hours", "parts"),
      ( 0,     18,      0),
      ( 0,      9,    204),
      ( 0,     15,    589),
      (29,     12,    793),
      ( 1,      5,    204)
    )

    forAll(data) {
      (days: Int, hours: Int, parts: Int) =>
        val moment = TimeVector().days(days).hours(hours).parts(parts)
        moment.days shouldBe days
        moment.hours shouldBe hours
        moment.parts shouldBe parts
    }
  }

  private val years = (1 to 6000) map (Year(_))

  "Jewish year" should "be the year of the month retrieved from it" in {
    for (year <- years; month <- year.months) month.year shouldBe year
  }

  // Shulchan Aruch, Orach Chaim, 428:1
  "Festivals" should "not fall on the proscribed days" in {
    years foreach { year =>
      def notOn(specialDay: SpecialDay, days: Day.Name*): Unit =
        days contains specialDay(year).name shouldBe false

      notOn(RoshHashanah, Rishon, Rvii, Shishi)
      notOn(YomKippur, Shlishi, Rishon, Shishi)
      notOn(Purim, Shabbos, Sheni, Rvii)
      notOn(Pesach, Sheni, Rvii, Shishi)
      notOn(Shavuot, Shlishi, Chamishi, Shabbos)
      notOn(HoshanahRabbah, Shlishi, Chamishi, Shabbos)
      notOn(Hanukkah1, Shlishi)
      notOn(FastOfEster, Rishon, Shlishi, Shishi)
      notOn(FastOfTammuz, Sheni, Rvii, Shishi)
      notOn(TishaBav, Sheni, Rvii, Shishi)

      def sameDay(a: SpecialDay, b: SpecialDay): Unit =
        a(year).name shouldBe b(year).name

      sameDay(Purim, LagBaOmer)
      // TODO This is wrong; see Taz
      // sameDay(Hanukkah, Shavuot)
    }
  }

  "Jewish Year" should "have allowed type with Pesach on correct day of the week" in {
    years foreach { year => Pesach(year).name shouldBe YearType.get(year).pesach }
  }
}
