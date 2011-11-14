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

import MonthName.MonthName


final class Year(val number: Int) {

    require(0 < number)


    override def equals(other: Any): Boolean = other match {
        case that: Year => (number == that.number)
        case _ => false
    }


    override def hashCode = number


    override def toString: String = number.toString


    def cycle: Int = ((number - 1) / Year.YearsInCycle) + 1


    def numberInCycle: Int = ((number - 1) % Year.YearsInCycle) + 1


    def isLeap: Boolean = Year.LeapYears.contains(numberInCycle)


    def lengthInMonths = if (isLeap) Year.MonthsInLeapYear else Year.MonthsInNonLeapYear


    def monthsBeforeInCycle: Int = Year.MonthsBeforeYearInCycle(numberInCycle-1)

    
    // First year was 0 month long!
    def monthsBefore: Int =
        Year.MonthsInCycle*(cycle-1) + monthsBeforeInCycle// - Year.lengthInMonth(1)


    def month(numberInYear: Int): Month = {
        require(0 < numberInYear && numberInYear <= lengthInMonths)
        Month(monthsBefore + numberInYear)
    }


    def month(name: MonthName): Month = month(Months.numberInYear(this, name))


    def length() = next.dayOfRoshHaShono.number - dayOfRoshHaShono.number


    def dayOfRoshHaShono: Day = {
        val newMoon = month(1).newMoon
        val day = newMoon.day
        val time = newMoon.time

        if (Year.isAdu(day)) day.next // KH 7:1
        else if (time.notEarlierThan(18, 0)) {
            if (!Year.isAdu(day.next)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
        }
        else if ((day.dayOfWeek == 3) && time.notEarlierThan(9, 204) && !this.isLeap) day.next.next /* KH 7:4 */
        else if ((day.dayOfWeek == 2) && time.notEarlierThan(15, 589) && this.prev.isLeap) day.next /* KH 7:5 */
        else day
    }


    // KH 8:7,8
    def kind: YearKind.YearKind = {
        val length = this.length

        def impossible = new IllegalArgumentException("Impossible year length " + length + " for " + this)

        import YearKind._
        if (!isLeap) {
            if (length == 353) Short else
            if (length == 354) Regular else
            if (length == 355) Full else
                throw impossible
        } else {
            if (length == 383) Short else
            if (length == 384) Regular else
            if (length == 385) Full else
                throw impossible
        }
    }


    def next: Year = Year(number + 1)


    def prev: Year = Year(number - 1)
}


object Year {

    val YearsInCycle = 19;


    val LeapYears = List(3, 6, 8, 11, 14, 17, 19)


    val MonthsInNonLeapYear = 12


    val MonthsInLeapYear = MonthsInNonLeapYear+1


    // TODO require(0 < _ < YearsInCycle)


    def yearMonthIsInCycle(number: Int): Int = MonthsBeforeYearInCycle.count(_ < number)


    private val MonthsBeforeYearInCycle = (1 to (YearsInCycle+1)) map {numberInCycle =>
        val leapYearsBeforeInCycle = LeapYears.count(_ < numberInCycle)
        leapYearsBeforeInCycle*MonthsInLeapYear + (numberInCycle - 1 - leapYearsBeforeInCycle)*MonthsInNonLeapYear
    }


    val MonthsInCycle = MonthsBeforeYearInCycle.last


    private val Adu = List(1, 4, 6)


    private def isAdu(day: Day) = Adu.contains(day.dayOfWeek)


    def apply(number: Int): Year = new Year(number)


    def apply(cycle: Int, numberInCycle: Int): Year = Year((cycle-1)*YearsInCycle + numberInCycle)
}
