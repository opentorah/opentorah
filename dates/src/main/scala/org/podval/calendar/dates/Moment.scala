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


final class Moment(val days: Int, val time: Time) {

    def day: Day = Day(days+1)


    override def equals(other: Any): Boolean = other match {
        case that: Moment => (days == that.days) && (time == that.time)
        case _ => false
    }


    override def hashCode = 41*days.hashCode + time.hashCode


    override def toString: String = days.toString + "d" + time.toString


    def toMinutesString: String = days.toString + "d" + time.toMinutesString

//    override def compare(that: Angle) = zip(that) map lift(_.compare(_)) find(_!= 0) getOrElse(0)


    def +(other: Moment) = Moment(
        days + other.days,
        time.hours + other.time.hours,
        time.parts + other.time.parts
    )


    def *(n: Int) = Moment(
        days*n,
        time.hours*n,
        time.parts*n
    )
}


object Moment {

    def apply(
        days: Int,
        hours: Int,
        parts: Int): Moment =
    {
        require(0 <= days)
        require(0 <= hours)
        require(0 <= parts)

        val hours_ = hours + parts/Time.PartsPerHour
    
        Moment(
           days + hours_ / Time.HoursPerDay,
           Time(hours_ % Time.HoursPerDay, parts % Time.PartsPerHour)
        )
    }
    

    def apply(day: Day, hours: Int, parts: Int): Moment = Moment(day.number-1, hours, parts)


    def apply(day: Day, time: Time): Moment = Moment(day.number-1, time)


    def apply(days: Int, time: Time) = new Moment(days, time)


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
