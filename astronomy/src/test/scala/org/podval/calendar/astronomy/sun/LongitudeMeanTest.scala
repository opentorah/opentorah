/*
 * Copyright 2011-2013 Podval Group.
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

import org.podval.calendar.astronomy.angle.Angle

import org.junit.Test
import org.junit.Assert


class LongitudeMeanTest {

  @Test
  def calculatedLessThenPrinted {
    LongitudeMean.keys.foreach{(days: Int) => Assert.assertTrue(days + "days", (days == 1) ||
      (LongitudeMean.calculated(days) < LongitudeMean.value(days)))}
  }


  @Test
  def almagestRoundsToRambam {
    Assert.assertEquals(LongitudeMean.rambamValue, Angle.roundToSeconds(LongitudeMean.almagestValue))
  }


  @Test
  def almagestRoundedLessThenPrinted {
    LongitudeMean.keys.foreach{(days: Int) => Assert.assertTrue(days + "days", (days <= 10) ||
      (Angle.roundToSeconds(LongitudeMean.almagest(days)) < LongitudeMean.value(days)))}
  }


  @Test
  def calculate29 {
    Assert.assertEquals(LongitudeMean.value(29),
      LongitudeMean.value(10)*3-LongitudeMean.value(1))
  }


  //@Test
  def calculate354 {
    Assert.assertEquals(LongitudeMean.value(354),
      LongitudeMean.value(100)*3+LongitudeMean.value(10)*5+LongitudeMean.value(1)*4)
  }
}
