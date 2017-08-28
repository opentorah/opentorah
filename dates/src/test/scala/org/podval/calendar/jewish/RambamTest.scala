package org.podval.calendar.jewish

import org.scalatest.FlatSpec
import org.podval.calendar.time.TimeNumberSystem.{hoursPerDay, partsPerHour}
import Jewish.{Year, Month, Day, interval, Moment, week}

/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RambamTest extends FlatSpec {

  "time units" should "be as in KH 6:2" in {
    assertResult(  24)(hoursPerDay)
    assertResult(1080)(partsPerHour)
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

  "molad Nisan example from KH 6:7" should "be correct" in {
    // TODO what is this and where should it be?
    //assert(Sun.firstMoladNisan.day.name == Day.Name.Rishon)
    //assert(Sun.firstMoladNisan.time == interval.hours(17).parts(107))

    // TODO Rambam doesn't mention the year, but it turns out to be 5066 - isn;t it kind of late for Rambam?!
    val moladNissn: Moment = Year(5066).month(Month.Name.Nisan).newMoon
    assertResult(interval.hours(17).parts(107))(moladNissn.time)
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
      Year(1).month(Month.Name.Nisan).newMoon - Sun.tkufasNisan(1))

    val vernalEquinox4930 = Sun.tkufasNisan(4930)
    assertResult(Day.Name.Chamishi)(vernalEquinox4930.day.name)
    assertResult(interval.hours(6))(vernalEquinox4930.time)
  }
}
