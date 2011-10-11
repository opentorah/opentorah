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


class Angle(val degrees: Int, val minutes: Int, val seconds: Int) {

    if (degrees < 0 || degrees >= 360) throw new IllegalArgumentException()
    if (minutes < 0 || minutes >= 60) throw new IllegalArgumentException()
    if (seconds < 0 || seconds >= 60) throw new IllegalArgumentException()


    def +(other: Angle): Angle = Angle(
        degrees + other.degrees,
        minutes + other.minutes,
        seconds + other.seconds
    )


    def *(n: Int): Angle = Angle(
        n*degrees,
        n*minutes,
        n*seconds
    )


    override def toString: String =
        degrees + "°" + minutes + "'" + seconds + "\""
}


object Angle {

    def apply(degrees: Int, minutes: Int): Angle =
        apply(degrees, minutes, 0)      


    def apply(degrees: Int, minutes: Int, seconds: Int): Angle = {
        val correctedMinutes = minutes + (seconds / 60)

        new Angle(
            toRange((degrees + correctedMinutes / 60), 360),
            toRange(correctedMinutes, 60),
            toRange(seconds, 60)
        )
    }


    private def toRange(what: Int, range: Int) = {
        val result = what % range
        if (result >= 0) result else result+range
    }
}
