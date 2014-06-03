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
    assert(Units.hoursPerDay == 24)
    assert(Units.partsPerHour == 1080)
  }


  "first two years' new moons" should "be correct" in {
    // KH 6:8
    val year1newMoon = Year(1).newMoon
    assert(year1newMoon.day.name == Day.Sheni)
    assert(year1newMoon.time == hours(5).parts(204))

    val year2newMoon = Year(2).newMoon
    assert(year2newMoon.day.name == Day.Shishi)
    assert(year2newMoon.time == hours(14))

    assert(year2newMoon - Month.meanLunarPeriod*12 == year1newMoon)

  }


  "molad Nisan example" should "be correct" ignore {
    // TODO KH 6:7
    val rambamTime = hours(17).parts(107)

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
    assert(Month.meanLunarPeriod == days(29).hours(12).parts(793))


    // KH 6:4
    assert(Month.meanLunarPeriod*12 == days(354).hours(8).parts(876))
    assert(Seasons.yearOfShmuel == days(365).hours(6))


    val week = days(7)

    // KH 6:5
    // TODO define constants "week", "normalYear", "leapYear"
    assert(Month.meanLunarPeriod % week == days(1).hours(12).parts(793))
    assert(Month.meanLunarPeriod*12 % week == days(4).hours(8).parts(876))
    assert(Month.meanLunarPeriod*13 % week == days(5).hours(21).parts(589))


    // KH 6:10
    assert(Seasons.yearOfShmuel*19 - Month.meanLunarPeriod*(12*12+7*13) == hours(1).parts(485))
    assert(Seasons.yearOfRavAda*19 - Month.meanLunarPeriod*(12*12+7*13) == hours(0).parts(  0))
    // TODO Year of Rav Ada == ...
  }


  "tkufos" should "be correct" in {
    assert(Year(1).month(Month.Nisan).newMoon - Seasons.tkufasNissan(1) == days(7).hours(9).parts(642))

    val vernalEquinox4930 = Seasons.tkufasNissan(4930)
    assert(vernalEquinox4930.day.name == Day.Chamishi)
    assert(vernalEquinox4930.time == hours(6))
  }
}
