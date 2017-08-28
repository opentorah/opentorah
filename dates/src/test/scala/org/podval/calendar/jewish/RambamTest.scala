package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import org.podval.calendar.time.TimeNumberSystem.{hoursPerDay, partsPerHour}
import Jewish.{Year, Month, Day, interval, week}

/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RambamTest extends FlatSpec {

  "time units" should "be as in KH 6:2" in {
    assertResult(  24)(hoursPerDay)
    assertResult(1080)(partsPerHour)
  }

  "first two years' new moons" should "be as in KH 6:8" in {
    val year1newMoon = Year(1).newMoon
    assertResult(Day.Name.Sheni)(year1newMoon.day.name)
    assertResult(interval.hours(5).parts(204))(year1newMoon.time)

    val year2newMoon = Year(2).newMoon
    assertResult(Day.Name.Shishi)(year2newMoon.day.name)
    assertResult(interval.hours(14))(year2newMoon.time)

    assertResult(year1newMoon)(year2newMoon - Month.meanLunarPeriod*12)
  }

  "molad Nisan example from KH 6:7" should "be correct" ignore {
    // TODO test fails!
    val rambamTime = interval.hours(17).parts(107)

    val years = for {
      number <- 1 to 6000
      year = Year(number)
      moladNisan = year.month(Month.Name.Nisan).newMoon
      if moladNisan.time == rambamTime
    //    assert(firstMoladNissan.day.name == Day.Rishon)
    //    assert(firstMoladNissan.time == hours(17).parts(107))
    } yield year

    // println(years)
  }

  "mean lunar period" should "be as in KH 6:3" in {
    assertResult(interval.days(29).hours(12).parts(793))(Month.meanLunarPeriod)
  }

  "year lengths" should "be as in KH 6:4" in {
    assertResult(interval.days(354).hours(8).parts(876))(Year.normal)
    assertResult(interval.days(365).hours(6)           )(Sun.yearOfShmuel)
  }

  "weekly reminders of month and year" should "be as in KH 6:5" in {
    assertResult(interval.days(1).hours(12).parts(793))(Month.meanLunarPeriod % week)
    assertResult(interval.days(4).hours( 8).parts(876))(Year.normal           % week)
    assertResult(interval.days(5).hours(21).parts(589))(Year.leap             % week)
  }

  "cycle reminder for year of Shmuel" should "be as in KH 6:10; 9:1" in {
    assertResult(interval.days(365).hours(6))(Sun.yearOfShmuel)
    assertResult(interval.hours(1).parts(485))(Sun.yearOfShmuel*Year.yearsInCycle - Year.cycleLength)
  }

  "cycle reminder for year of Rav Ada" should "be as in KH 10:1" in {
    assertResult(interval.days(365).hours(5).parts(997).moments(48))(Sun.yearOfRavAda)
    assertResult(interval)(Sun.yearOfRavAda*Year.yearsInCycle - Year.cycleLength)
  }

  // TODO more tkusfos
  // TODO reference to KH?
  "tkufos" should "be correct" in {
    assertResult(interval.days(7).hours(9).parts(642))(
      Year(1).month(Month.Name.Nisan).newMoon - Sun.tkufasNissan(1))

    val vernalEquinox4930 = Sun.tkufasNissan(4930)
    assertResult(Day.Name.Chamishi)(vernalEquinox4930.day.name)
    assertResult(interval.hours(6))(vernalEquinox4930.time)
  }
}
