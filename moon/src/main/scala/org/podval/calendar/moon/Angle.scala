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


class Angle(val degrees: Int, val minutes: Int, val seconds: Int, val thirds: Int) {

    if (degrees < 0 || degrees >= 360) throw new IllegalArgumentException()
    if (minutes < 0 || minutes >= 60) throw new IllegalArgumentException()
    if (seconds < 0 || seconds >= 60) throw new IllegalArgumentException()
    if (thirds < 0 || thirds >= 60) throw new IllegalArgumentException()


    def +(other: Angle): Angle = Angle(
        degrees + other.degrees,
        minutes + other.minutes,
        seconds + other.seconds,
        thirds + other.thirds
    )


    def *(n: Int): Angle = Angle(
        n*degrees,
        n*minutes,
        n*seconds,
        n*thirds
    )


    override def toString: String =
      degrees + "°" + minutes + "′" + seconds + "″" + (if (thirds == 0) "" else thirds + "‴")
}


object Angle {

    def apply(degrees: Int, minutes: Int): Angle =
        apply(degrees, minutes, 0)      


    def apply(degrees: Int, minutes: Int, seconds: Int): Angle =
        apply(degrees, minutes, seconds, 0)      


    def apply(degrees: Int, minutes: Int, seconds: Int, thirds: Int): Angle = {
        val seconds_ = carry(seconds, thirds)
        val minutes_ = carry(minutes, seconds_)
        val degrees_ = carry(degrees, minutes_)

        new Angle(
            toRange(degrees_, 360),
            toRange(minutes_, 60),
            toRange(seconds_, 60),
            toRange(thirds, 60)
        )
    }


    private def carry(big: Int, small: Int): Int =
        big + (small / 60)


    private def toRange(what: Int, range: Int) = {
        val result = what % range
        if (result >= 0) result else result+range
    }
}
