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

package org.podval.calendar.astronomy.moon

import org.junit.Test
import org.junit.Assert
import org.podval.calendar.astronomy.angle.Angle


class AnomalyVisibleTest {

    @Test
    def misprinted() {
        test(AnomalyVisible.MISPRINTED)
    }


    @Test
    def correct() {
        test(AnomalyVisible.VALUES)
    }


    private def test(table: Map[Angle, Angle]) = {
        for (row <- table) {
            val maslul: Angle = row._1
            val mnas: Angle = row._2
            val e: Double = AnomalyVisible.efrommnasround(maslul, mnas)
            val mnas_ = AnomalyVisible.mnasfrome(maslul, e).roundToMinutes

            Assert.assertEquals(mnas, mnas_)
        }
    }
}
