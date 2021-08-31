package org.opentorah.calendar

import org.opentorah.calendar.jewish.{Jewish, NewYear, YearType}
import org.opentorah.calendar.jewish.SpecialDay._
import Jewish.{Day, Month, TimeVector, Year}
import Jewish.Month._
import org.opentorah.calendar.roman.Gregorian
import Gregorian.Month._
import Week.Day._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}

final class DatesTest extends AnyFlatSpec with Matchers {

  "Days of the Week" should "load correctly" in {
    Week.Day.Rishon.names.hasName("воскресенье") shouldBe true
  }

  "Known dates" should "have correct day of the week" in {
    Year(5772).month(Marheshvan).day(24).name shouldBe Sheni
    // Father's yahrtzeit: println(Gregorian.Year(2014).month(April).day(24).to(Jewish))
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
      (5773, Nisan     , Shlishi ,  5773, Nisan  ,  1,  2013, March    , 12,  6, 21, 14),

      (5781, Nisan     , Sunday  ,  5781, Nisan  ,  1,  2021, March    , 13, 19,  3,  5), // chabad.org incorrectly lists Saturday?
    )

    forAll(data) {
      (
        moladYear: Int, moladMonth: Month.Name, dayOfWeek: Week.Day,
        year: Int, month: Month.Name, day: Int,
        yearG: Int, monthG: Gregorian.Month.Name, dayG: Int,
        hours: Int, minutes: Int, parts: Int
      ) =>
        val dayJ = Year(year).month(month).day(day)

        val molad = Year(moladYear).month(moladMonth).newMoon

        molad.day.name shouldBe dayOfWeek
        molad.day      shouldBe dayJ

        val dateG = Gregorian.Year(yearG).month(monthG).day(dayG).toMoment.
          hours(hours).minutes(minutes).partsWithoutMinutes(parts)

        Gregorian.Moment.from(molad.asInstanceOf[Calendar#Moment]) shouldBe dateG
        molad.to(Gregorian) shouldBe dateG

        dateG.to(Jewish   ).day shouldBe dayJ
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

  private val yearsWithDelaysEnabled = years.drop(NewYear.delaysEnabledFromYear-1)

  "Jewish year" should "be the year of the month retrieved from it" in {
    for (year <- years; month <- year.months) month.year shouldBe year
  }

  // Shulchan Aruch, Orach Chaim, 428:1
  "Festivals" should "not fall on the proscribed days" in {
    val data = Table[Year => Day, Seq[Week.Day]](
      ("specialDay", "notOn"),
      (RoshHashanah1.date, Seq(Rishon, Rvii, Shishi)),
      (YomKippur.date, Seq(Shlishi, Rishon, Shishi)),
      (Purim.date, Seq(Shabbos, Sheni, Rvii)),
      (Pesach1.date, Seq(Sheni, Rvii, Shishi)),
      (Shavuos1.date, Seq(Shlishi, Chamishi, Shabbos)),
      (HoshanahRabbah.date, Seq(Shlishi, Chamishi, Shabbos)),
      (Chanukah1.date, Seq(Shlishi)),
      (FastOfEster.date, Seq(Rishon, Shlishi, Shishi)),
      (FastOfTammuz.date, Seq(Sheni, Rvii, Shishi)),
      (TishaBeAv.date, Seq(Sheni, Rvii, Shishi))
    )
    yearsWithDelaysEnabled foreach { year =>
      forAll(data) {
        (specialDay: Year => Day, days: Seq[Week.Day]) =>
        days contains specialDay(year).name shouldBe false
      }

      Purim.date(year).name shouldBe LagBaOmer.date(year).name
      // misprint, see Taz 1, Magen Avraham 1
      // Chanukah(year).name shouldBe Shavuos(year.name)
    }
  }

//  "Purim" should "fall on Friday from time to time" in {
//    years foreach { year =>
//      if (Purim.date(year).name == Day.Name.Shishi) println(year)
//    }
//  }

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
      (AdarI, Seq(Sheni, Rvii, Chamishi, Shabbos)), // => Parshas Shekalim never falls on Rosh Chodesh!
      (AdarII, Seq(Sheni, Rvii, Shishi, Shabbos))
    )
    yearsWithDelaysEnabled foreach { year =>
      forAll(data) {
        (month: Month.Name, days: Seq[Week.Day]) =>
          if (year.containsMonth(month)) {
            val roshChodesDay = year.month(month).firstDay
            days contains roshChodesDay.name shouldBe true
          }
      }
    }
  }

  // Shulchan Aruch, Orach Chaim, 428:3
  "Festivals" should "satisfy the rules" in {
    years foreach { year =>
      val pesach: Day = Pesach1.date(year)
      TishaBeAv.date(year).name shouldBe pesach.name
      Shavuos1.date(year).name shouldBe (pesach+1).name
      RoshHashanah1.date(year+1).name shouldBe (pesach+2).name
      SimchasTorah.date(year+1).name shouldBe (pesach+3).name
      YomKippur.date(year+1).name shouldBe (pesach+4).name
      Purim.date(year).name shouldBe (pesach+5).name
    }
  }

  "Jewish Year" should "have allowed type with Pesach on correct day of the week" in {
    yearsWithDelaysEnabled foreach(year => Pesach1.date(year).name shouldBe YearType.forYear(year).pesach)
  }

  "Giving of the Torah" should "be off by the fixed calendar :)" in {
    // Revelation took place in year 2448, on Shabbos, Sivan 6th (or 7th according to Rabbi Jose).
    // Jews came to Mount Sinai on new moon, which was on Monday (or Sunday).
    // Fixed calendar gives the new moon of the month on Wednesday and the 6th on the following Wednesday
    // (or Tuesday if New Year push-out are suppressed),
    // proving that the fixed calendar was not in use then ;)
    val day = Year(2448).month(Sivan).day(6)
    day.month.newMoon.day.name shouldBe Rvii
    day.name shouldBe Rvii

    // TODO calculate based on the astronomical calendar!
  }

  "Pesach day of the week" should "calculate correctly" in {
    val days: collection.mutable.Map[Week.Day, Int] = new collection.mutable.HashMap[Week.Day, Int]
    years.foreach { year =>
      val day: Week.Day = Pesach1.date(year).name
      if (days.contains(day)) days.update(day, days(day) + 1) else days.update(day, 1)
    }
    //println(days)
  }
}
