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


class MultiplicationTable(val angles: Map[Int, Angle]) {

    val approximate: Angle = angles(1)


    val exactInDegrees: Double = exactify(approximate, 10000, angles(10000))


    val exact = Angle.fromDegrees(exactInDegrees, 6)


    final def print() {
        for (days <- List(1, 10, 100, 1000, 10000, 29, 354)) {
            val angle = angles(days)
            val exacter = Angle.fromDegrees(exactify(approximate, days, angle), 6)
            println(days + ":" + angle + ":" + exact*days + ":" + exacter)
        }
    }


    def write(writer: TableWriter) {
        writer.addColumn("days")
        writer.addColumn("approximate")
        writer.addColumn("exact")
        writer.addColumn("reverse")

        for (days <- List(1, 10, 100, 1000, 10000, 29, 354)) {
            val angle = angles(days)
            val exacter = Angle.fromDegrees(exactify(approximate, days, angle), 6)
            writer.startRow
            writer.value(days)
            writer.value(angle)
            writer.value(exact*days)
            writer.value(exacter)
        }

        writer.endTable
    }


    def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
        val daysForFullRotation = 360.0/approximate.toDegrees
        val fullRotations = scala.math.floor(days/daysForFullRotation).toInt
        (360.0*fullRotations+angle.toDegrees)/days
    }
}
