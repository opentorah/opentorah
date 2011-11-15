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

package org.podval.calendar.dates


final class Time private (val hours: Int, val parts: Int) extends Ordered[Time] {

    require(0 <= hours && hours < Time.HoursPerDay)
    require(0 <= parts && parts < Time.PartsPerHour)


    override def equals(other: Any): Boolean = other match {
        case that: Time => this.allParts == that.allParts
        case _ => false
    }


    override def hashCode = 41*hours+parts


    override def compare(that: Time) = this.allParts - that.allParts


    def isZero = (hours == 0) && (parts == 0)


    def allParts = hours*Time.PartsPerHour + parts


    def minutes: Int = parts / Time.PartsPerMinute


    def partsOfMinute = parts % Time.PartsPerMinute


    override def toString: String = hours + "h" + parts + "p"


    def toMinutesString: String = hours + "h" + minutes + "m" + partsOfMinute + "p"
}


object Time {

    val HoursPerDay = 24


    require(HoursPerDay % 2 == 0)


    val HoursPerHalfDay = HoursPerDay / 2


    val PartsPerHour = 1080


    val MinutesPerHour = 60


    require(PartsPerHour % MinutesPerHour == 0)


    val PartsPerMinute = PartsPerHour / MinutesPerHour


    def apply(hours: Int, parts: Int) = new Time(hours, parts)


    def ofNight(hours: Int, parts: Int) = {
        require(hours < HoursPerHalfDay)
        Time(hours, parts)
    }


    def ofDay(hours: Int, parts: Int) = {
        require(hours < HoursPerHalfDay)
        Time(hours + HoursPerHalfDay, parts)
    }
}
