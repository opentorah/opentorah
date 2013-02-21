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

package org.podval.calendar.dates


final class Day private (number: Int) extends Numbered[Day](number) {

    require(0 < number)


    def dayOfWeek: Int = Day.dayOfWeek(number)


    def dayOfMonth: Int = number - month.firstDay + 1


    def dayOfYear: Int = number - year.firstDay + 1


    def year: Year = Year(this)


    def month: Month = year.month(this)


    def next: Day = Day(number + 1)


    def prev: Day = Day(number - 1)


    def time(time: Time): Moment = Moment(number - 1, time)


    def time(hours: Int, parts: Int): Moment = time(Time(hours, parts))


    def nightTime(hours: Int, parts: Int): Moment = time(Time.nightTime(hours, parts))


    def dayTime(hours: Int, parts: Int): Moment = time(Time.dayTime(hours, parts))


    def toFullString: String = year + " " + month.name + " " + dayOfMonth 
}


object Day {

    // It seems that first day of the first year was Sunday.
    val FirstDayDayOfWeek = 1


    val DaysPerWeek = 7


    def dayOfWeek(day: Int): Int = ((day + FirstDayDayOfWeek - 1 - 1) % DaysPerWeek) + 1


    def apply(number: Int): Day = new Day(number)
}
