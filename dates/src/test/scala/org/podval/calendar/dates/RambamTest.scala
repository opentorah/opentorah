/*
 * Copyright 2014 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.calendar.dates

import org.scalatest.FlatSpec

import Jewish._


/**
 * Tests based on the statements from the text itself.
 */
@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RambamTest extends FlatSpec {

  "time units" should "be correct" in {
    val j = Jewish // WTF?!

    // KH 6:2
    assert(TimeNumberSystem.hoursPerDay == 24)
    assert(TimeNumberSystem.partsPerHour == 1080)
  }


  "first two years' new moons" should "be correct" in {
    // KH 6:8
    val year1newMoon = Year(1).newMoon
    assert(year1newMoon.day.name == Day.Sheni)
    assert(year1newMoon.time == interval.hours(5).parts(204))

    val year2newMoon = Year(2).newMoon
    assert(year2newMoon.day.name == Day.Shishi)
    assert(year2newMoon.time == interval.hours(14))

    assert(year2newMoon - Month.meanLunarPeriod*12 == year1newMoon)

  }


  "molad Nisan example" should "be correct" ignore {
    // TODO KH 6:7
    val rambamTime = interval.hours(17).parts(107)

    val years = for {
      number <- (1 to 6000)
      year = Year(number)
      moladNisan = year.month(Month.Nisan).newMoon
      if (moladNisan.time == rambamTime)
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
    assert(Seasons.yearOfShmuel == interval.days(365).hours(6))


    // KH 6:5
    assert(Month.meanLunarPeriod % week == interval.days(1).hours(12).parts(793))
    assert(Year.normal           % week == interval.days(4).hours( 8).parts(876))
    assert(Year.leap             % week == interval.days(5).hours(21).parts(589))


    // KH 6:10; 9:1
    assert(Seasons.yearOfShmuel == interval.days(365).hours(6))
    assert(Seasons.yearOfShmuel*Year.yearsInCycle - Year.cycleLength == interval.hours(1).parts(485))


    // KH 10:1
    assert(Seasons.yearOfRavAda == interval.days(365).hours(5).parts(997).moments(48))
    assert(Seasons.yearOfRavAda*Year.yearsInCycle - Year.cycleLength == interval)
  }


  "tkufos" should "be correct" in {
    assert(Year(1).month(Month.Nisan).newMoon - Seasons.tkufasNissan(1) == interval.days(7).hours(9).parts(642))

    val vernalEquinox4930 = Seasons.tkufasNissan(4930)
    assert(vernalEquinox4930.day.name == Day.Chamishi)
    assert(vernalEquinox4930.time == interval.hours(6))
  }
}
