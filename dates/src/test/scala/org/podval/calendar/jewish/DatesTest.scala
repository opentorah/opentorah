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
    Day(5772, Marheshvan, 24).name shouldBe Sheni
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
    val data = Table(
      ("specialDay", "notOn"),
      (RoshHashanah1, Seq(Rishon, Rvii, Shishi)),
      (YomKippur, Seq(Shlishi, Rishon, Shishi)),
      (Purim, Seq(Shabbos, Sheni, Rvii)),
      (Pesach, Seq(Sheni, Rvii, Shishi)),
      (Shavuos, Seq(Shlishi, Chamishi, Shabbos)),
      (HoshanahRabbah, Seq(Shlishi, Chamishi, Shabbos)),
      (Chanukah1, Seq(Shlishi)),
      (FastOfEster, Seq(Rishon, Shlishi, Shishi)),
      (FastOfTammuz, Seq(Sheni, Rvii, Shishi)),
      (TishaBeAv, Seq(Sheni, Rvii, Shishi))
    )
    years foreach { year =>
      forAll(data) {
        (specialDay: SpecialDay, days: Seq[Day.Name]) =>
        days contains specialDay(year).name shouldBe false
      }

      Purim(year).name shouldBe SpecialDay.lagBaOmer(year).name
      // misprint, see Taz 1, Magen Avraham 1
      // Hanukkah(year).name shouldBe Shavuot(year.name)
    }
  }

  // Shulchan Aruch, Orach Chaim, 428:2
  "Roshei Chodoshim" should "fall on allowed days" in {
    val data = Table(
      ("month", "days"),
      (Nisan, Seq(Rishon, Shlishi, Chamishi, Shabbos)),
      (Iyar, Seq(Sheni, Shlishi, Chamishi, Shabbos)),
      (Sivan, Seq(Rishon, Shlishi, Rvii, Shishi)),
      (Tammuz, Seq(Rishon, Shlishi, Chamishi, Shishi)),
      (Av, Seq(Sheni, Rvii, Shishi, Shabbos)),
      (Elul, Seq(Rishon, Sheni, Rvii, Shishi)),
      (Tishrei, Seq(Sheni, Shlishi, Chamishi, Shabbos)),
      // see Taz 2, Magen Avraham 2:
      (Marheshvan, Seq(Sheni, Rvii, Chamishi, Shabbos)),
      (Kislev, Seq(Rishon, Sheni, Shlishi, Rvii, Chamishi, Shishi)),
      (Teves, Seq(Rishon, Sheni, Shlishi, Rvii, Shishi)),
      (Shvat, Seq(Sheni, Shlishi, Rvii, Chamishi, Shabbos)),
      // see Ramo:
      (Adar, Seq(Shabbos, Sheni, Rvii, Shishi)),
      (AdarI, Seq(Sheni, Rvii, Chamishi, Shabbos)),
      (AdarII, Seq(Sheni, Rvii, Shishi, Shabbos))
    )
    years foreach { year =>
      forAll(data) {
        (month: Month.Name, days: Seq[Day.Name]) =>
          val monthExistsInYear: Boolean =
            (month != Adar && month != AdarI && month != AdarII) ||
            ((month == Adar) && !year.isLeap) ||
            ((month == AdarI || month == AdarII) && year.isLeap)
          if (monthExistsInYear) {
            val roshChodesDay = year.month(month).firstDay
            days contains roshChodesDay.name shouldBe true
          }
      }
    }
  }

  // Shulchan Aruch, Orach Chaim, 428:3
  "Festivals" should "follow the rule" in {
    years foreach { year =>
      val pesach: Day = Pesach(year)
      TishaBeAv(year).name shouldBe pesach.name
      Shavuos(year).name shouldBe (pesach+1).name
      RoshHashanah1(year+1).name shouldBe (pesach+2).name
      SimchasTorah(year+1).name shouldBe (pesach+3).name
      YomKippur(year+1).name shouldBe (pesach+4).name
      Purim(year).name shouldBe (pesach+5).name
    }
  }


  "Jewish Year" should "have allowed type with Pesach on correct day of the week" in {
    years foreach { year => Pesach(year).name shouldBe YearType.get(year).pesach }
  }
}
