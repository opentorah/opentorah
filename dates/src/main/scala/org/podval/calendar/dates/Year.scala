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


final class Year(val number: Int) {

    def cycle: Int = Year.cycle(number)


    def numberInCycle: Int = Year.numberInCycle(number)


    def isLeap: Boolean = Year.isLeapYear(number)


    def lengthInMonth = Year.lengthInMonth(numberInCycle)


    def monthsBeforeInCycle: Int = {
        val result = Year.monthsBeforeYearInCycle(numberInCycle)
        // First year woth 0 month long!
        if (cycle == 1) result - Year.lengthInMonth(1) else result
    }

    def monthsBefore: Int = Year.MonthsInCycle*(cycle-1) + monthsBeforeInCycle


    def month(month: Int): Month = Month(this, month)


    def yearLength() = Year(number+1).dayOfRoshHaShono - dayOfRoshHaShono


    def dayOfRoshHaShono: Int = {
        val newMoon = month(1).newMoon
        val day = newMoon.day
        val dayOfTheWeek = Time.dayOfTheWeek(day)

        if (Year.isAdu(dayOfTheWeek)) day+1 // KH 7:1
        else if (newMoon.notEarlierInTheDayThan(18, 0)) {
            if (!Year.isAdu(Time.dayOfTheWeek(day+1)))
            day+1 /* KH 7:2 */ else
            day+2 /* KH 7:3 */
        }
        else if ((dayOfTheWeek == 3) && newMoon.notEarlierInTheDayThan(9, 240) &&
            !Year.isLeapYear(number))  day+2 /* KH 7:4 */
        else if ((dayOfTheWeek == 2) && newMoon.notEarlierInTheDayThan(15, 589) &&
            Year.isLeapYear(number-1)) day+1 /* KH 7:5 */
        else day
    }
}


object Year {

    val YearsInCycle = 19;


    val LeapYears = List(3, 6, 8, 11, 14, 17, 19)


    val LeapYearsInCycle = LeapYears.length


    val NonLeapYearsInCycle = YearsInCycle - LeapYearsInCycle


    val MonthsInNonLeapYear = 12


    val MonthsInLeapYear = MonthsInNonLeapYear+1


    def cycle(number: Int): Int = (number / YearsInCycle) + 1


    def numberInCycle(number: Int): Int = number % YearsInCycle


    // TODO require(0 < _ < YearsInCycle)


    def isLeapYear(number: Int) = isLeapYearInCycle(numberInCycle(number))


    def isLeapYearInCycle(numberInCycle: Int) = LeapYears.contains(numberInCycle)


    def lengthInMonth(numberInCycle: Int) = if (isLeapYearInCycle(numberInCycle)) Year.MonthsInLeapYear else Year.MonthsInNonLeapYear


    def leapYearsBeforeInCycle(numberInCycle: Int): Int = LeapYears.count(_ < numberInCycle)


    def nonLeapYearsBeforeInCycle(numberInCycle: Int): Int = numberInCycle - 1 - leapYearsBeforeInCycle(numberInCycle)


    def monthsBeforeYearInCycle(numberInCycle: Int): Int =
        leapYearsBeforeInCycle(numberInCycle)*MonthsInLeapYear +
        nonLeapYearsBeforeInCycle(numberInCycle)*MonthsInNonLeapYear


    val MonthsBeforeYearInCycle = (1 to (YearsInCycle+1)) map (monthsBeforeYearInCycle(_))


    val MonthsInCycle = MonthsBeforeYearInCycle.last


    def yearMonthIsInCycle(number: Int): Int = MonthsBeforeYearInCycle.count(_ < number)


    private val Adu = List(1, 4, 6)


    private def isAdu(dayOfTheWeek: Int) = Adu.contains(dayOfTheWeek)


    def apply(number: Int): Year = new Year(number)


    def apply(cycle: Int, numberInCycle: Int): Year = Year((cycle-1)*YearsInCycle + numberInCycle)
}
