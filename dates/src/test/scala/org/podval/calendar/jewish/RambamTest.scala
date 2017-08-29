package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import org.podval.calendar.time.TimeNumberSystem.{hoursPerDay, hoursPerHalfDay, partsPerHour}
import Jewish.{Year, Month, Day, interval, Moment, week}
import Month.meanLunarPeriod

/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RambamTest extends FlatSpec {

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
    assertResult(interval.days(365).hours(6))(Sun.yearOfShmuel)
    assertResult(interval.days(10).hours(21).parts(204))(Sun.yearOfShmuel - Year.normal)
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

  "cycle reminder for year of Shmuel" should "be as in KH 6:10; 9:1" in {
    assertResult(19)(Year.yearsInCycle)
    assertResult( 7)(Year.leapYearsInCycle)
    assertResult(interval.days(365).hours(6))(Sun.yearOfShmuel)
    assertResult(Year.normal*12 + Year.leap*7)(Year.cycleLength)
    assertResult(interval.hours(1).parts(485))(Sun.yearOfShmuel*Year.yearsInCycle - Year.cycleLength)
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



  "cycle reminder for year of Rav Ada" should "be as in KH 10:1" in {
    assertResult(interval.days(365).hours(5).parts(997).moments(48))(Sun.yearOfRavAda)
    assertResult(interval)(Sun.yearOfRavAda*Year.yearsInCycle - Year.cycleLength)
  }

  // TODO more tkusfos
  // TODO reference to KH?
  "tkufos" should "be correct" in {
    assertResult(interval.days(7).hours(9).parts(642))(
      Year(1).month(Month.Name.Nisan).newMoon - Sun.tkufasNisan(1))

    val vernalEquinox4930 = Sun.tkufasNisan(4930)
    assertResult(Day.Name.Chamishi)(vernalEquinox4930.day.name)
    assertResult(interval.hours(6))(vernalEquinox4930.time)
  }
}
