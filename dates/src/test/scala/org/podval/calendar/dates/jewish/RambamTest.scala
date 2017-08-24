package org.podval.calendar.dates.jewish

import org.scalatest.FlatSpec
import Jewish.{Year, Month, Day, interval, week}

/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RambamTest extends FlatSpec {

  "first two years' new moons" should "be correct" in {
    // KH 6:8
    val year1newMoon = Year(1).newMoon
    assertResult(Day.Name.Sheni)(year1newMoon.day.name)
    assertResult(interval.hours(5).parts(204))(year1newMoon.time)

    val year2newMoon = Year(2).newMoon
    assertResult(Day.Name.Shishi)(year2newMoon.day.name)
    assertResult(interval.hours(14))(year2newMoon.time)

    assertResult(year1newMoon)(year2newMoon - Month.meanLunarPeriod*12)
  }

  "molad Nisan example" should "be correct" ignore {
    // TODO test fails!
    // KH 6:7
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

  "time intervals" should "be correct" in {
    val j = Jewish // WTF?!

    // KH 6:3
    assert(Month.meanLunarPeriod == interval.days(29).hours(12).parts(793))

    // KH 6:4
    assert(Year.normal == interval.days(354).hours(8).parts(876))
    assert(Sun.yearOfShmuel == interval.days(365).hours(6))

    // KH 6:5
    assert(Month.meanLunarPeriod % week == interval.days(1).hours(12).parts(793))
    assert(Year.normal           % week == interval.days(4).hours( 8).parts(876))
    assert(Year.leap             % week == interval.days(5).hours(21).parts(589))

    // KH 6:10; 9:1
    assert(Sun.yearOfShmuel == interval.days(365).hours(6))
    assert(Sun.yearOfShmuel*Year.yearsInCycle - Year.cycleLength == interval.hours(1).parts(485))

    // KH 10:1
    assert(Sun.yearOfRavAda == interval.days(365).hours(5).parts(997).moments(48))
    assert(Sun.yearOfRavAda*Year.yearsInCycle - Year.cycleLength == interval)
  }

  "tkufos" should "be correct" in {
    assert(Year(1).month(Month.Name.Nisan).newMoon - Sun.tkufasNissan(1) ==
      interval.days(7).hours(9).parts(642))

    val vernalEquinox4930 = Sun.tkufasNissan(4930)
    assert(vernalEquinox4930.day.name == Day.Name.Chamishi)
    assert(vernalEquinox4930.time == interval.hours(6))
  }
}
