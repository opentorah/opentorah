/*
 * Copyright 2011 Podval Group.
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

package org.podval.calendar.moon


trait MultiplicationTable {

    val PRINTED: Map[Int, Angle]


    val Rambam: Angle


    val RambamExact: Angle


    def print() {
        for ((days, angle) <- PRINTED) {
            val exact = Angle.fromDegrees(exactify(Rambam, days, angle), 6)
            println(days + ":" + angle + ":" + RambamExact*days + ":" + exact)
        }
    }


    def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
        val daysForFullRotation = 360.0/approximate.toDegrees
        val fullRotations = scala.math.floor(days/daysForFullRotation).toInt
        (360.0*fullRotations+angle.toDegrees)/days
    }
}
