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


final class DayG private (number: Int) extends Numbered[DayG](number) {

    def dayOfWeek: Int = Day.dayOfWeek(toJewishDayNumber)


    def dayOfMonth: Int = number - month.firstDay + 1


    def dayOfYear: Int = number - year.firstDay + 1


    def year: YearG = YearG(this)


    def month: MonthG = year.month(this)


    def next: DayG = DayG(number + 1)


    def prev: DayG = DayG(number - 1)


    def time(time: TimeG): MomentG = MomentG(number - 1, time)


    def time(hours: Int, parts: Int): MomentG = time(TimeG(hours, parts))


    def morningTime(hours: Int, parts: Int): MomentG = time(TimeG.morningTime(hours, parts))


    def afternoonTime(hours: Int, parts: Int): MomentG = time(TimeG.afternoonTime(hours, parts))


    def toJewish = Day(toJewishDayNumber)


    private def toJewishDayNumber = number + DayG.Epoch


    def toFullString: String = year + " " + month.name + " " + dayOfMonth 
}


object DayG {

    val Epoch = 1373429


    def apply(number: Int): DayG = new DayG(number)


    def fromJewish(day: Day) = DayG(day.number - Epoch)
}
