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


final class Day(val number: Int) {

    require(0 < number)


    override def equals(other: Any): Boolean = other match {
        case that: Day => (number == that.number)
        case _ => false
    }


    override def hashCode = number


    override def toString: String = number.toString


    def dayOfWeek: Int = ((number - 1) % 7) + 1


    def year: Year = {
        val yearForSureBefore = (4 * number / (4 * 365 + 1)) - 1
        var result = Year(yearForSureBefore)
        require(result.dayOfRoshHaShono.number < number)
        while (result.next.dayOfRoshHaShono.number > number) result = result.next
        result
    }


    def month: Month = year.month(number - year.monthsBefore)


    def next: Day = Day(number + 1)


    def prev: Day = Day(number - 1)
}


object Day {

    def apply(number: Int): Day = new Day(number)
}
