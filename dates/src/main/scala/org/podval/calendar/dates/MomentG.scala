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


final class MomentG private (days: Int, time: TimeG) extends MomentT[TimeG, MomentG](days, time) {

    def day: DayG = DayG(days + 1)


    def create(days: Int, hours: Int, parts: Int): MomentG = MomentG(days, TimeG(hours, parts))


    def toJewish: Moment = {
        val hours = time.hours

        val (newDay, newHours) =
            if (hours >= MomentG.JewishDayStartHours)
                (day.next, hours - MomentG.JewishDayStartHours) else
                (day     , hours + MomentG.JewishDayLeftHours)

        newDay.toJewish.time(newHours, time.parts)
    }


    def toFullString: String = day.toFullString + " " + time.toFullString
}


object MomentG {

    val JewishDayStartHours = 18


    val JewishDayLeftHours = TimeT.HoursPerDay - JewishDayStartHours


    def apply(days: Int, time: TimeG) = new MomentG(days, time)


    def fromJewish(moment: Moment): MomentG = {
        val day = moment.day
        val hours = moment.time.hours

        val (newDay, newHours) =
            if (hours < MomentG.JewishDayLeftHours)
                (day.prev, hours + MomentG.JewishDayStartHours) else
                (day     , hours - MomentG.JewishDayLeftHours)

        DayG.fromJewish(newDay).time(newHours, moment.time.parts)
    }
}
