package org.podval.calendar.jewish

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.times.Times.{hoursPerDay, hoursPerHalfDay, partsPerHour, momentsPerPart}
import Jewish.{Day, Month, Year, TimeVector, range, week}
import JewishYearCompanion.{normalYear, leapYear}
import Moon.meanLunarPeriod
import Sun.{yearOfShmuel, yearOfRavAda}

/**
 * Tests based on the statements from the text itself.
 */
class TextTest extends FlatSpec with Matchers {
  "time units" should "be as in KH 6:2" in {
    hoursPerDay shouldBe 24
    range(0) shouldBe 24
    hoursPerHalfDay shouldBe 12
    partsPerHour shouldBe 1080
    range(1) shouldBe 1080
    partsPerHour % 2 shouldBe 0
    partsPerHour % 4 shouldBe 0
    partsPerHour % 8 shouldBe 0
    partsPerHour % 3 shouldBe 0
    partsPerHour % 6 shouldBe 0
    partsPerHour % 9 shouldBe 0
    partsPerHour % 10 shouldBe 0
    momentsPerPart shouldBe 76
    range(2) shouldBe 76
  }

  "mean lunar period" should "be as in KH 6:3" in {
    meanLunarPeriod shouldBe TimeVector().days(29).hours(12).parts(793)
  }

  "year lengths" should "be as in KH 6:4" in {
    normalYear shouldBe meanLunarPeriod*12
    normalYear shouldBe TimeVector().days(354).hours( 8).parts(876)

    leapYear shouldBe meanLunarPeriod*13
    leapYear shouldBe TimeVector().days(383).hours(21).parts(589)

    // (see also KH 9:1, 10:6)
    yearOfShmuel shouldBe TimeVector().days(365).hours(6)
    (yearOfShmuel - normalYear) shouldBe TimeVector().days(10).hours(21).parts(204)
  }

  "weekly reminders of month and year" should "be as in KH 6:5" in {
    (meanLunarPeriod % week) shouldBe TimeVector().days(1).hours(12).parts(793)
    (normalYear      % week) shouldBe TimeVector().days(4).hours( 8).parts(876)
    (leapYear        % week) shouldBe TimeVector().days(5).hours(21).parts(589)
  }

  "molad Nisan example from KH 6:7" should "be correct" in {
    (TimeVector().hours(17).parts(107) + meanLunarPeriod).time shouldBe
     TimeVector().hours( 5).parts(900)
  }

  "first two years' new moons" should "be as in KH 6:8" in {
    val year1newMoon = Year(1).newMoon
    year1newMoon.day.name shouldBe Day.Name.Sheni
    // see also KH 6:13
    year1newMoon.time shouldBe TimeVector().hours(5).parts(204)

    val year2newMoon = Year(2).newMoon
    year2newMoon.day.name shouldBe Day.Name.Shishi
    year2newMoon.time shouldBe TimeVector().hours(14)

    (year2newMoon - meanLunarPeriod*12) shouldBe year1newMoon
  }

  "year of Shmuel" should "be as in KH 6:10; 9:1-2" in {
    Cycle.yearsInCycle shouldBe 19
    Cycle.leapYearsInCycle shouldBe 7
    yearOfShmuel shouldBe TimeVector().days(365).hours(6)
    Cycle.cycleLength shouldBe (normalYear*12 + leapYear*7)
    (yearOfShmuel*Cycle.yearsInCycle - Cycle.cycleLength) shouldBe
      TimeVector().hours(1).parts(485)
    // KH 9:2
    SeasonsFixed.Shmuel.seasonLength shouldBe TimeVector().days(91).hours(7).halfHour
  }

  "leap years" should "be as in KH 6:11" in {
    Cycle.leapYears shouldBe Set(3, 6, 8, 11, 14, 17, 19)
  }

  "cycle remainder" should "be as in KH 6:12" in {
    (TimeVector().days(4).hours( 8).parts(876)*12 +
     TimeVector().days(5).hours(21).parts(589)* 7) % week shouldBe
     TimeVector().days(2).hours(16).parts(595)
  }

  "kind of the year" should "be correct for years from KH 8:9" in {
    var numberRegular: Int = 0
    var numberFull: Int = 0
    var numberShort: Int = 0
    for (yearNumber <- 2 to 6000) {
      val year: Year = Year(yearNumber)
      if (!year.isLeap) {
        val nextYearDay: Day.Name = year.next.firstDay.name
        if (year.firstDay.is(Day.Name.Chamishi)) {
          if (nextYearDay == Day.Name.Sheni) {
            numberRegular += 1
            year.kind shouldBe Year.Kind.Regular
          }
          if (nextYearDay == Day.Name.Shlishi) {
            numberFull += 1
            year.kind shouldBe Year.Kind.Full
          }
        }
        if (year.firstDay.is(Day.Name.Shabbos)) {
          if (nextYearDay == Day.Name.Shlishi) {
            numberShort += 1
            year.kind shouldBe Year.Kind.Short
          }
        }
      }
    }

    // Verify that we saw some years with properties from the text
    assert(numberRegular > 0) // 1084 of them!
    assert(numberFull    > 0) // 198 of them!
    assert(numberShort   > 0) // 259 of them!
  }

  "year kind laws" should "be as in KH 8:10" in {
    import Day.Name._
    for (yearNumber <- 1 to 6000) {
      val year = Year(yearNumber)
      val roshHashono = year.firstDay
      roshHashono.name match {
        case Shlishi  => assert(Year.Kind.Regular == year.kind)
        case Shabbos  => assert(Year.Kind.Regular != year.kind)
        case Sheni    => assert(Year.Kind.Regular != year.kind)
        case Chamishi =>
          if (year.isLeap) assert(Year.Kind.Regular != year.kind)
          else assert(Year.Kind.Short != year.kind)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  "first tkufas Nisan for Shmuel" should "be as in KH 9:3-4" in {
    SeasonsFixed.firstMoladNisan shouldBe Year(1).month(Month.Name.Nisan).newMoon
    (SeasonsFixed.firstMoladNisan - SeasonsFixed.Shmuel.firstTkufasNisan) shouldBe
      TimeVector().days(7).hours(9).parts(642)
    SeasonsFixed.Shmuel.firstTkufasNisan.day.name shouldBe Day.Name.Rvii
  }

  "tkufos of 4930" should "be as in KH 9:5-7" in {
    val year: Year = Year(4930)

    year.cycle shouldBe 260
    year.numberInCycle shouldBe 9

    val tkufasNisan = SeasonsFixed.Shmuel.tkufasNisan(year)
    tkufasNisan.day.name shouldBe Day.Name.Chamishi
    tkufasNisan.time shouldBe TimeVector().hours(6)
    tkufasNisan.day shouldBe year.month(Month.Name.Nisan).day(8)

    val tkufasTammuz = SeasonsFixed.Shmuel.tkufasTammuz(year)
    tkufasTammuz.day.name shouldBe Day.Name.Chamishi
    tkufasTammuz.time shouldBe TimeVector().hours(13).halfHour

    val tkufasTishrei = SeasonsFixed.Shmuel.tkufasTishrei(year)
    tkufasTishrei.day.name shouldBe Day.Name.Chamishi
    tkufasTishrei.time shouldBe TimeVector().hours(21)

    val tkufasTeves = SeasonsFixed.Shmuel.tkufasTeves(year)
    tkufasTeves.day.name shouldBe Day.Name.Shishi
    tkufasTeves.time shouldBe TimeVector().hours(4).halfHour

    val nextTkufasNisan = SeasonsFixed.Shmuel.tkufasNisan(year+1)
    nextTkufasNisan.day.name shouldBe Day.Name.Shishi
    nextTkufasNisan.time shouldBe TimeVector().hours(12)
  }

  "year of RavAda" should "be as in KH 10:1-2" in {
    yearOfRavAda shouldBe TimeVector().days(365).hours(5 ).parts(997).moments(48)
    (yearOfRavAda - normalYear) shouldBe
      TimeVector().days( 10).hours(21).parts(121).moments(48)
    (yearOfRavAda*Cycle.yearsInCycle - Cycle.cycleLength) shouldBe TimeVector.zero
    // KH 10:2
    SeasonsFixed.RavAda.seasonLength shouldBe
      TimeVector().days(91).hours(7).parts(519).moments(31)
  }

  "first tkufas Nisan for RavAda" should "as in KH 10:3" in {
    (SeasonsFixed.firstMoladNisan - SeasonsFixed.RavAda.firstTkufasNisan) shouldBe
      TimeVector().hours(9).parts(642)
    SeasonsFixed.RavAda.firstTkufasNisan.day.name shouldBe Day.Name.Rvii
  }
}
