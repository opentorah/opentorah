package org.opentorah.calendar

import org.opentorah.calendar.jewish.{Jewish, NewYear}
import org.opentorah.calendar.roman.{Gregorian, Julian}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

final class EpochTest extends AnyFlatSpec, Matchers:

  "First Jewish year (of Chaos)" should "be correct" in {
    val year1: Jewish.Year = Jewish.Year(1)

    val year1newMoon: Jewish.Moment = year1.newMoon
    year1newMoon.day.name shouldBe Week.Day.Sheni
    year1newMoon.time     shouldBe Jewish.TimeVector().hours(5).parts(204) // BaHaRaD

    val year1firstDay: Jewish.Day = year1.firstDay
    year1firstDay.numberInYear shouldBe 1
    year1firstDay.number shouldBe 1

    // Rosh Hashano on the day of molad
    year1newMoon.day shouldBe year1firstDay

    year1.newYearDelay shouldBe NewYear.Delay.No // even when not suppressed
  }

  "Second Jewish year (of Creation)" should "be correct" in {
    val year2: Jewish.Year = Jewish.Year(2)

    val year2newMoon: Jewish.Moment = year2.newMoon
    year2newMoon.day.name shouldBe Week.Day.Shishi
    year2newMoon.time shouldBe Jewish.TimeVector().hours(14) // WeYaD 8:00am

    val year2firstDay: Jewish.Day = year2.firstDay
    year2firstDay.numberInYear shouldBe 1

    // Note: I had to suppress NewYear.delay() for years 2, 3 and 4 to recover the desired
    // outcome: molad of the year 2 falling out on the first of Tishrei;
    // this is not cheating: delays were not instituted for thousands of years...

    // Rosh Hashano on the day of molad
    year2newMoon.day shouldBe year2firstDay

    year2.newYearDelay shouldBe NewYear.Delay.No // when suppresed; Adu otherwise
  }

  "Gregorian.epoch" should "be correct" in {
    val epoch: Gregorian.Day = Gregorian.Year(1).month(Gregorian.Month.January).day(1)

    epoch.year.number shouldBe 1
    epoch.year.firstDayNumber shouldBe 1

    epoch.month.name shouldBe Gregorian.Month.January
    epoch.month.number shouldBe 1
    epoch.month.firstDayNumber shouldBe 1

    epoch.numberInMonth shouldBe 1

    epoch.name shouldBe Week.Day.Monday

    epoch           .number shouldBe 1
    epoch.to(Jewish).number shouldBe (Gregorian.epoch+1)
  }

  "Julian.epoch" should "be correct" in {
    val epoch: Julian.Day = Julian.Year(1).month(Julian .Month.January).day(1)

    epoch.year.number shouldBe 1
    epoch.year.firstDayNumber shouldBe 1

    epoch.month.name shouldBe Julian.Month.January
    epoch.month.number shouldBe 1
    epoch.month.firstDayNumber shouldBe 1

    epoch.numberInMonth shouldBe 1

    epoch.name shouldBe Week.Day.Saturday

    epoch           .number shouldBe 1
    epoch.to(Jewish).number shouldBe (Julian.epoch+1)
  }

  "Roman epochs" should "be consistent" in {
    // Julian 1,1,3 = Gregorian 1,1,1    (Monday)
    // Julian 1,1,1 = Gregorian 0,12,30

    val gregorian: Gregorian.Day = Gregorian.Year(1).month(Gregorian.Month.January).day(1)
    gregorian.name shouldBe Week.Day.Monday

    val julian   : Julian   .Day = Julian   .Year(1).month(Julian   .Month.January).day(3)
    julian   .name shouldBe Week.Day.Monday

    julian   .to(Gregorian) shouldBe gregorian
    gregorian.to(Julian   ) shouldBe julian
  }

  "Calendrical Calculations Epoch" should "work" in {
    val example: Gregorian.Day = Gregorian.Day(710347)
    example            shouldBe Gregorian.Year(1945).month(Gregorian.Month.November).day(12)
    example            shouldBe Julian   .Year(1945).month(Julian   .Month.October ).day(30).to(Gregorian)
    example.to(Julian) shouldBe Julian   .Year(1945).month(Julian   .Month.October ).day(30)
    example.to(Jewish) shouldBe Jewish   .Year(5706).month(Jewish   .Month.Kislev  ).day( 7)
  }

  "Russian switch dates" should "convert correctly" in {
    // In Russia, the Gregorian calendar was accepted after the October Revolution.
    // On 24 January 1918 the Council of People's Commissars issued a decree that Wednesday,
    // 31 January 1918, was to be followed by Thursday, 14 February 1918, thus dropping 13 days from the calendar.
    // With the change, the October Revolution itself (October 25 1917 Julian), once converted, took place on 7 November.

    val lastJulian: Julian.Day = Julian.Year(1918).month(Julian.Month.January).day(31)
    lastJulian.name shouldBe Week.Day.Wednesday

    val firstGregorian: Gregorian.Day = Gregorian.Year(1918).month(Gregorian.Month.February).day(14)
    firstGregorian.name shouldBe Week.Day.Thursday

    firstGregorian.to(Julian) shouldBe Julian.Year(1918).month(Julian.Month.February).day(1)
    firstGregorian.to(Julian).name shouldBe Week.Day.Thursday

    val revolution: Julian.Day = Julian.Year(1917).month(Julian.Month.October).day(25)
    revolution.to(Gregorian) shouldBe Gregorian.Year(1917).month(Gregorian.Month.November).day(7)
  }

  "Julian Day" should "be correct" in {
    // The dates and days Meeus lists are Julian before 1582-10-04 Julian (1582-10-15 Gregorian) and Gregorian after.

    Gregorian.Year( 2000).month(Gregorian.Month.January ).day(  1).toMoment.hours(12).toJulianDay shouldBe 2451545.0
    Gregorian.Year( 2000).month(Gregorian.Month.January ).day(  1).toMoment.hours(12).to(Jewish).toJulianDay shouldBe 2451545.0
    Gregorian.Year( 1999).month(Gregorian.Month.January ).day(  1).toMoment                 .toJulianDay shouldBe 2451179.5
    Gregorian.Year( 1988).month(Gregorian.Month.June    ).day( 19).toMoment.hours(12).toJulianDay shouldBe 2447332.0
    Gregorian.Year( 1988).month(Gregorian.Month.January ).day( 27).toMoment                 .toJulianDay shouldBe 2447187.5
    Gregorian.Year( 1987).month(Gregorian.Month.June    ).day( 19).toMoment.hours(12).toJulianDay shouldBe 2446966.0
    Gregorian.Year( 1987).month(Gregorian.Month.January ).day( 27).toMoment                 .toJulianDay shouldBe 2446822.5
    Gregorian.Year( 1900).month(Gregorian.Month.January ).day(  1).toMoment                 .toJulianDay shouldBe 2415020.5
    Gregorian.Year( 1858).month(Gregorian.Month.November).day( 17).toMoment                 .toJulianDay shouldBe 2400000.5
    Gregorian.Year( 1600).month(Gregorian.Month.December).day( 31).toMoment                 .toJulianDay shouldBe 2305812.5
    Gregorian.Year( 1600).month(Gregorian.Month.January ).day(  1).toMoment                 .toJulianDay shouldBe 2305447.5
    Julian.Year(  837).month(Julian.Month.April   ).day(10).toMoment          .toJulianDay shouldBe 2026871.5
    Julian.Year(- 122).month(Julian.Month.January ).day( 1).toMoment          .toJulianDay shouldBe 1676497.5
    Julian.Year(- 123).month(Julian.Month.December).day(31).toMoment          .toJulianDay shouldBe 1676496.5
    Julian.Year(-1000).month(Julian.Month.July    ).day(12).toMoment.hours(12).toJulianDay shouldBe 1356001.0
    Julian.Year(-1000).month(Julian.Month.February).day(29).toMoment          .toJulianDay shouldBe 1355866.5
    Julian.Year(-1001).month(Julian.Month.August  ).day(17).toMoment          .toJulianDay shouldBe 1355670.5
    Julian.Year(-4712).month(Julian.Month.January ).day( 1).toMoment.hours(12).toJulianDay shouldBe       0.0
    Gregorian.Year(-4713).month(Gregorian.Month.November).day(24).toMoment.hours(12).toJulianDay shouldBe       0.0
    Gregorian.Year(-4713).month(Gregorian.Month.November).day(24).toMoment.hours(12).to(Jewish).toJulianDay shouldBe       0.0
  }
