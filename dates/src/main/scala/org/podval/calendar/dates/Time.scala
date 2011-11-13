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


final class Time(
    val days: Int,
    val hours: Int,
    val parts: Int)
{
    require(0 <= days)
    require(0 <= hours && hours < 24)
    require(0 <= parts && parts < 1080)


    def minutes: Int = parts / 18


    def partsOfMinute = parts % 18


    def day = if ((hours == 0) && (parts == 0)) days else days+1


    def dayOfWeek = Time.dayOfTheWeek(day)


    override def equals(other: Any): Boolean = other match {
        case that: Time =>
            (days == that.days) && (hours == that.hours) && (parts == that.parts)
        case _ => false
    }


    override def hashCode = 41*(41*(41+days)+hours)+parts


    override def toString: String = days + "d" + hours + "h" + parts + "p"


    def toMinutesString: String = days + "d" + hours + "h" + minutes + "m" + partsOfMinute + "p"


//    override def compare(that: Angle) = zip(that) map lift(_.compare(_)) find(_!= 0) getOrElse(0)


    def notEarlierInTheDayThan(hours: Int, parts: Int) =
        (this.hours > hours) || ((this.hours == hours) && (this.parts >= parts))


    def +(other: Time) = Time(days+other.days, hours+other.hours, parts+other.parts)


    def *(n: Int) = Time(days*n, hours*n, parts*n)
}


object Time {

    def dayOfTheWeek(day: Int) = ((day-1) % 7) + 1


    def apply(
        days: Int = 0,
        hours: Int = 0,
        parts: Int = 0) =
{
    require(0 <= days)
    require(0 <= hours)
    require(0 <= parts)

    val hours_ = hours + parts/1080
    
    new Time(days + hours_ / 24, hours_ % 24, parts % 1080)
}
    
    
//    def toJewishTime: Moment =
//        val (newDays, newHours) =
//            if (hours >= 18) (days+1, hours-18) else (days, hours+6)
//
//        Moment(newDays, Flavour.Secular, newHours, minutes, seconds)
//    }
//
//
//    def toSecularTime: Moment =
//        val (newDays, newHours) =
//            if (hours < 6) (days-1, hours+18) else (days, hours-6)
//
//        Moment(newDays, Flavour.Secular, newHours, minutes, seconds)
//    }
}
