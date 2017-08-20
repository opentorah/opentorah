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

package org.podval.calendar.astronomy.moon

import org.scalatest.FlatSpec

import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AnomalyVisibleTest extends FlatSpec {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(AnomalyVisible.MISPRINTED)
  }


  it should "be correct" in {
    test(AnomalyVisible.VALUES)
  }


  private def test(table: Map[Angle, Angle]) = {
    for (row <- table) {
      val (maslul, mnas) = row
      val e: Double = AnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = AnomalyVisible.mnasfrome(maslul, e)
      val mnas_ = mnasfrome.roundToMinutes

      assert(mnas == mnas_)
    }
  }
}
