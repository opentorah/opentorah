/*
 * Copyright 2011-2014 Podval Group.
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

package org.podval.calendar.astronomy.sun
import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle
import org.podval.calendar.dates.Sun

import org.scalatest.FlatSpec


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LongitudeMeanTest extends FlatSpec {

  behavior of "Mean Longitude"

  it should "calculate to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days == 1) ||
      (LongitudeMean.calculated(days) < LongitudeMean.value(days)))}
  }


  it should "be what Almagest rounds to" in {
    assertResult(LongitudeMean.rambamValue)(LongitudeMean.almagestValue.roundToSeconds)
  }


  it should "round from Almagest to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days <= 10) ||
      (LongitudeMean.almagest(days).roundToSeconds < LongitudeMean.value(days)))}
  }


  it should "calculate for 29 days in two steps" in {
    assertResult(LongitudeMean.value(29))(LongitudeMean.value(10)*3-LongitudeMean.value(1))
  }


  it should "calculate correctly for the regular year" ignore {
    assertResult(LongitudeMean.value(354))(LongitudeMean.value(100)*3+LongitudeMean.value(10)*5+LongitudeMean.value(1)*4)
  }


  it should "make a full circle in a year" in {
    println(LongitudeMean.exact_ * Sun.yearOfRavAda)
    println(LongitudeMean.exact_ * Sun.yearOfShmuel)
  }
}
