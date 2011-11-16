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


final class Day private (val number: Int) extends Ordered[Day] {

    require(0 < number)


    override def equals(other: Any): Boolean = other match {
        case that: Day => (number == that.number)
        case _ => false
    }


    override def hashCode = number


    override def compare(that: Day) = this.number - that.number


    override def toString: String = number.toString


    // TODO give names to constants
    def dayOfWeek: Int = ((number - 1) % 7) + 1 // TODO: what was the day of the week of the first day of the first year?


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
}


object Day {

    def apply(number: Int): Day = new Day(number)
}
