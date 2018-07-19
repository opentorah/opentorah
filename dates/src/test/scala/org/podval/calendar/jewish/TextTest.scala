package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import org.podval.calendar.time.TimeNumberSystem.{hoursPerDay, hoursPerHalfDay, partsPerHour, momentsPerPart}
import Jewish.{Day, Month, Year, TimeVector, range, week}
import JewishYearCompanion.{normalYear, leapYear}
import Moon.meanLunarPeriod
import Sun.{yearOfShmuel, yearOfRavAda}

/**
 * Tests based on the statements from the text itself.
 */
class TextTest extends FlatSpec {
  "time units" should "be as in KH 6:2" in {
    assertResult(  24)(hoursPerDay)
    assertResult(  24)(range(0))
    assertResult(  12)(hoursPerHalfDay)
    assertResult(1080)(partsPerHour)
    assertResult(1080)(range(1))
    assertResult(0)(partsPerHour % 2)
    assertResult(0)(partsPerHour % 4)
    assertResult(0)(partsPerHour % 8)
    assertResult(0)(partsPerHour % 3)
    assertResult(0)(partsPerHour % 6)
    assertResult(0)(partsPerHour % 9)
    assertResult(0)(partsPerHour % 10)
    assertResult(76)(momentsPerPart)
    assertResult(76)(range(2))
  }

  "mean lunar period" should "be as in KH 6:3" in {
    assertResult(TimeVector().days(29).hours(12).parts(793))(meanLunarPeriod)
  }

  "year lengths" should "be as in KH 6:4" in {
    assertResult(normalYear)(meanLunarPeriod*12)
    assertResult(TimeVector().days(354).hours( 8).parts(876))(normalYear)

    assertResult(leapYear)(meanLunarPeriod*13)
    assertResult(TimeVector().days(383).hours(21).parts(589))(leapYear)

    // (see also KH 9:1, 10:6)
    assertResult(TimeVector().days(365).hours(6))(yearOfShmuel)
    assertResult(TimeVector().days(10).hours(21).parts(204))(yearOfShmuel - normalYear)
  }

  "weekly reminders of month and year" should "be as in KH 6:5" in {
    assertResult(TimeVector().days(1).hours(12).parts(793))(meanLunarPeriod % week)
    assertResult(TimeVector().days(4).hours( 8).parts(876))(normalYear      % week)
    assertResult(TimeVector().days(5).hours(21).parts(589))(leapYear        % week)
  }

  "molad Nisan example from KH 6:7" should "be correct" in {
    assertResult(TimeVector().hours( 5).parts(900))(
      (TimeVector().hours(17).parts(107) + meanLunarPeriod).time)
  }

  "first two years' new moons" should "be as in KH 6:8" in {
    val year1newMoon = Year(1).newMoon
    assertResult(Day.Name.Sheni)(year1newMoon.day.name)
    // see also KH 6:13
    assertResult(TimeVector().hours(5).parts(204))(year1newMoon.time)

    val year2newMoon = Year(2).newMoon
    assertResult(Day.Name.Shishi)(year2newMoon.day.name)
    assertResult(TimeVector().hours(14))(year2newMoon.time)

    assertResult(year1newMoon)(year2newMoon - meanLunarPeriod*12)
  }

  "year of Shmuel" should "be as in KH 6:10; 9:1-2" in {
    assertResult(19)(Cycle.yearsInCycle)
    assertResult( 7)(Cycle.leapYearsInCycle)
    assertResult(TimeVector().days(365).hours(6))(yearOfShmuel)
    assertResult(normalYear*12 + leapYear*7)(Cycle.cycleLength)
    assertResult(TimeVector().hours(1).parts(485))(
      yearOfShmuel*Cycle.yearsInCycle - Cycle.cycleLength)
    // KH 9:2
    assertResult(TimeVector().days(91).hours(7).halfHour)(SeasonsFixed.Shmuel.seasonLength)
  }

  "leap years" should "be as in KH 6:11" in {
    assertResult(Set(3, 6, 8, 11, 14, 17, 19))(Cycle.leapYears)
  }

  "cycle remainder" should "be as in KH 6:12" in {
    assertResult(TimeVector().days(2).hours(16).parts(595))(
      (TimeVector().days(4).hours( 8).parts(876)*12 +
        TimeVector().days(5).hours(21).parts(589)* 7) % week
    )
  }

  "kind of the year" should "be correct for years from KH 8:9" in {
    var numberRegular: Int = 0
    var numberFull: Int = 0
    var numberShort: Int = 0
    for (yearNumber <- 2 to 6000) {
      val year: Year = Year(yearNumber)
      if (!year.isLeap) {
        val nextYearDay: Day.Name = year.next.firstDay.name
        if (year.firstDay.name == Day.Name.Chamishi) {
          if (nextYearDay == Day.Name.Sheni) {
            numberRegular += 1
            assertResult(Year.Kind.Regular)(year.kind)
          }
          if (nextYearDay == Day.Name.Shlishi) {
            numberFull += 1
            assertResult(Year.Kind.Full)(year.kind)
          }
        }
        if (year.firstDay.name == Day.Name.Shabbos) {
          if (nextYearDay == Day.Name.Shlishi) {
            numberShort += 1
            assertResult(Year.Kind.Short)(year.kind)
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
    assertResult(Year(1).month(Month.Name.Nisan).newMoon)(SeasonsFixed.firstMoladNisan)
    assertResult(TimeVector().days(7).hours(9).parts(642))(
      SeasonsFixed.firstMoladNisan - SeasonsFixed.Shmuel.firstTkufasNisan)
    assertResult(Day.Name.Rvii)(SeasonsFixed.Shmuel.firstTkufasNisan.day.name)
  }

  "tkufos of 4930" should "be as in KH 9:5-7" in {
    val year: Year = Year(4930)

    assertResult(260)(year.cycle)
    assertResult(9)(year.numberInCycle)

    val tkufasNisan = SeasonsFixed.Shmuel.tkufasNisan(year)
    assertResult(Day.Name.Chamishi)(tkufasNisan.day.name)
    assertResult(TimeVector().hours(6))(tkufasNisan.time)
    assertResult(year.month(Month.Name.Nisan).day(8))(tkufasNisan.day)

    val tkufasTammuz = SeasonsFixed.Shmuel.tkufasTammuz(year)
    assertResult(Day.Name.Chamishi)(tkufasTammuz.day.name)
    assertResult(TimeVector().hours(13).halfHour)(tkufasTammuz.time)

    val tkufasTishrei = SeasonsFixed.Shmuel.tkufasTishrei(year)
    assertResult(Day.Name.Chamishi)(tkufasTishrei.day.name)
    assertResult(TimeVector().hours(21))(tkufasTishrei.time)

    val tkufasTeves = SeasonsFixed.Shmuel.tkufasTeves(year)
    assertResult(Day.Name.Shishi)(tkufasTeves.day.name)
    assertResult(TimeVector().hours(4).halfHour)(tkufasTeves.time)

    val nextTkufasNisan = SeasonsFixed.Shmuel.tkufasNisan(year+1)
    assertResult(Day.Name.Shishi)(nextTkufasNisan.day.name)
    assertResult(TimeVector().hours(12))(nextTkufasNisan.time)
  }

  "year of RavAda" should "be as in KH 10:1-2" in {
    assertResult(TimeVector().days(365).hours(5 ).parts(997).moments(48))(yearOfRavAda)
    assertResult(TimeVector().days( 10).hours(21).parts(121).moments(48))(yearOfRavAda - normalYear)
    assertResult(TimeVector())(yearOfRavAda*Cycle.yearsInCycle - Cycle.cycleLength)
    // KH 10:2
    assertResult(TimeVector().days(91).hours(7).parts(519).moments(31))(
      SeasonsFixed.RavAda.seasonLength)
  }

  "first tkufas Nisan for RavAda" should "as in KH 10:3" in {
    assertResult(TimeVector().hours(9).parts(642))(
      SeasonsFixed.firstMoladNisan - SeasonsFixed.RavAda.firstTkufasNisan)
    assertResult(Day.Name.Rvii)(SeasonsFixed.RavAda.firstTkufasNisan.day.name)
  }
}
