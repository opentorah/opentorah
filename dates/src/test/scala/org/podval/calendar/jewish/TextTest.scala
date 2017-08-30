package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import org.podval.calendar.time.TimeNumberSystem.{hoursPerDay, hoursPerHalfDay, partsPerHour}
import Jewish.{Day, Moment, Month, Year, interval}
import Sun.{yearOfShmuel, yearOfRavAda}
import Month.meanLunarPeriod
import org.podval.calendar.dates.Calendar

/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class TextTest extends FlatSpec {
  final val week: Jewish#TimeInterval = interval.days(Calendar.daysPerWeek)

  "time units" should "be as in KH 6:2" in {
    assertResult(  24)(hoursPerDay)
    assertResult(  12)(hoursPerHalfDay)
    assertResult(1080)(partsPerHour)
    assertResult(0)(partsPerHour % 2)
    assertResult(0)(partsPerHour % 4)
    assertResult(0)(partsPerHour % 8)
    assertResult(0)(partsPerHour % 3)
    assertResult(0)(partsPerHour % 6)
    assertResult(0)(partsPerHour % 9)
    assertResult(0)(partsPerHour % 10)
  }

  "mean lunar period" should "be as in KH 6:3" in {
    assertResult(interval.days(29).hours(12).parts(793))(meanLunarPeriod)
  }

  "year lengths" should "be as in KH 6:4" in {
    assertResult(Year.normal)(meanLunarPeriod*12)
    assertResult(interval.days(354).hours( 8).parts(876))(Year.normal)

    assertResult(Year.leap)(meanLunarPeriod*13)
    assertResult(interval.days(383).hours(21).parts(589))(Year.leap)

    // (see also KH 9:1, 10:6)
    assertResult(interval.days(365).hours(6))(yearOfShmuel)
    assertResult(interval.days(10).hours(21).parts(204))(yearOfShmuel - Year.normal)
  }

  "weekly reminders of month and year" should "be as in KH 6:5" in {
    assertResult(interval.days(1).hours(12).parts(793))(meanLunarPeriod % week)
    assertResult(interval.days(4).hours( 8).parts(876))(Year.normal     % week)
    assertResult(interval.days(5).hours(21).parts(589))(Year.leap       % week)
  }

  "molad Nisan example from KH 6:7" should "be correct" in {
    // Rambam doesn't give the year; the only year with molad Nisan on the time he gives is 5066,
    // but day of the week is Rvii instead of Rishon :(
    val year: Year = Year(5066)
    val moladNisan: Moment = year.month(Month.Name.Nisan).newMoon
    assertResult(interval.hours(17).parts(107))(moladNisan.time)
//    assertResult(Day.Name.Rishon)(moladNisan.day.name)
    val moladIyar: Moment = moladNisan.day.month.next.newMoon
    assertResult(interval.hours( 5).parts(900))(moladIyar.time)
  }

  "first two years' new moons" should "be as in KH 6:8" in {
    val year1newMoon = Year(1).newMoon
    assertResult(Day.Name.Sheni)(year1newMoon.day.name)
    // see also KH 6:13
    assertResult(interval.hours(5).parts(204))(year1newMoon.time)

    val year2newMoon = Year(2).newMoon
    assertResult(Day.Name.Shishi)(year2newMoon.day.name)
    assertResult(interval.hours(14))(year2newMoon.time)

    assertResult(year1newMoon)(year2newMoon - Month.meanLunarPeriod*12)
  }

  "year of Shmuel" should "be as in KH 6:10; 9:1-2" in {
    assertResult(19)(Year.yearsInCycle)
    assertResult( 7)(Year.leapYearsInCycle)
    assertResult(interval.days(365).hours(6))(yearOfShmuel)
    assertResult(Year.normal*12 + Year.leap*7)(Year.cycleLength)
    assertResult(interval.hours(1).parts(485))(yearOfShmuel*Year.yearsInCycle - Year.cycleLength)
    // KH 9:2
    assertResult(interval.days(91).hours(7).parts(partsPerHour/2))(Seasons.Shmuel.seasonLength)
  }

  "leap years" should "be as in KH 6:11" in {
    assertResult(Set(3, 6, 8, 11, 14, 17, 19))(Year.leapYears)
  }

  "cycle remainder" should "be as in KH 6:12" in {
    assertResult(interval.days(2).hours(16).parts(595))(
      (interval.days(4).hours( 8).parts(876)*12 +
      interval.days(5).hours(21).parts(589)*7) % week
    )
  }

  // TODO tests for KH 8:9

  // KH 8:10
  "year kind laws" should "be as in KH 8:10" in {
    for (yearNumber <- 1 to 6000) {
      val year = Year(yearNumber)
      val roshHashono = year.firstDay
      roshHashono.name match {
        case Day.Name.Shlishi  => assertResult(Year.Kind.Regular)(year.kind)
        case Day.Name.Shabbos  => assert(Year.Kind.Regular != year.kind)
        case Day.Name.Sheni    => assert(Year.Kind.Regular != year.kind)
        case Day.Name.Chamishi =>
          if (year.isLeap) assert(Year.Kind.Regular != year.kind)
          else assert(Year.Kind.Short != year.kind)
      }
    }
  }

  "first tkufas Nisan for Shmuel" should "as in KH 9:3-4" in {
    assertResult(Year(1).month(Month.Name.Nisan).newMoon)(Sun.firstMoladNisan)
    assertResult(interval.days(7).hours(9).parts(642))(
      Sun.firstMoladNisan - Seasons.Shmuel.firstTkufasNisan)
    // TODO more tests from 9:4
    assertResult(Day.Name.Rvii)(Seasons.Shmuel.firstTkufasNisan.day.name)
  }

  "tkufos of 4930" should "be as in KH 9:5-8" in {
    val year: Year = Year(4930)
    val tkufasNisan = Seasons.Shmuel.tkufasNisan(year)
    assertResult(Day.Name.Chamishi)(tkufasNisan.day.name)
    assertResult(interval.hours(6))(tkufasNisan.time)

    val tkufasTammuz = Seasons.Shmuel.tkufasTammuz(year)
    assertResult(Day.Name.Chamishi)(tkufasTammuz.day.name)
    assertResult(interval.hours(13).parts(partsPerHour/2))(tkufasTammuz.time)

    val tkufasTishrei = Seasons.Shmuel.tkufasTishrei(year)
    assertResult(Day.Name.Chamishi)(tkufasTishrei.day.name)
    assertResult(interval.hours(21))(tkufasTishrei.time)

    val tkufasTeves = Seasons.Shmuel.tkufasTeves(year)
    assertResult(Day.Name.Shishi)(tkufasTeves.day.name)
    assertResult(interval.hours(4).parts(partsPerHour/2))(tkufasTeves.time)

    val nextTkufasNisan = Seasons.Shmuel.tkufasNisan(year+1)
    assertResult(Day.Name.Shishi)(nextTkufasNisan.day.name)
    assertResult(interval.hours(12))(nextTkufasNisan.time)

    // TODO add "halfHour" as an alias for parts(partsPerHpur/2)?
    // TODO more tests from KH 9:6-8
  }

  "year of RavAda" should "be as in KH 10:1-2" in {
    assertResult(interval.days(365).hours(5).parts(997).moments(48))(yearOfRavAda)
    assertResult(interval.days(10).hours(21).parts(121).moments(48))(yearOfRavAda - Year.normal)
    assertResult(interval)(yearOfRavAda*Year.yearsInCycle - Year.cycleLength)
    // KH 10:2
    assertResult(interval.days(91).hours(7).parts(519).moments(31))(Seasons.RavAda.seasonLength)
  }

  "first tkufas Nisan for RavAda" should "as in KH 10:3-4" in {
    assertResult(interval.hours(9).parts(642))(Sun.firstMoladNisan - Seasons.RavAda.firstTkufasNisan)
    // TODO more tests from 10:3-4
    assertResult(Day.Name.Rvii)(Seasons.RavAda.firstTkufasNisan.day.name)
  }

  // TODO KH 10:7 test that real vernal equinox is approximately two days before the mean one
}
