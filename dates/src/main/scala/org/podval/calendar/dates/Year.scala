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
import YearKind._


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


    def isLeap: Boolean = Year.isLeap(numberInCycle)


    def lengthInMonths = Year.lengthInMonths(numberInCycle)


    def month(numberInYear: Int): Month = {
        require(0 < numberInYear && numberInYear <= lengthInMonths)
        Month(monthsBefore + numberInYear)
    }


    def month(name: MonthName): Month = month(months.indexWhere(_.name == name) + 1)


    def monthOfDay(numberInYear: Int): Month = {
        require(0 < numberInYear && numberInYear <= length)
        month(months.count(_.daysBefore < numberInYear))
    }


    def monthsBefore: Int = Year.MonthsInCycle*(cycle - 1) + monthsBeforeInCycle


    def monthsBeforeInCycle: Int = Year.MonthsBeforeYearInCycle(numberInCycle - 1)


    def length() = next.firstDay - this.firstDay


    def firstDay: Int = {
        val newMoon = month(1).newMoon
        val day = newMoon.day
        val time = newMoon.time

        if (Year.isAdu(day)) day.next // KH 7:1
        else if (time >= Time(18, 0)) {
            if (!Year.isAdu(day.next)) day.next /* KH 7:2 */ else day.next.next /* KH 7:3 */
        }
        else if ((day.dayOfWeek == 3) && time >= Time( 9, 204) && !this.isLeap) day.next.next /* KH 7:4 */
        else if ((day.dayOfWeek == 2) && time >= Time(15, 589) && this.prev.isLeap) day.next /* KH 7:5 */
        else day
    }.number


    // KH 8:7,8
   def kind: YearKind = {
        val daysOverShort = length - (if (isLeap) 383 else 353)

        daysOverShort match {
        case 0 => Short
        case 1 => Regular
        case 2 => Full
        case _ => throw new IllegalArgumentException("Impossible year length " + length + " for " + this)
        }
    }


    def months = Months.months(this)


    def next: Year = Year(number + 1)


    def prev: Year = Year(number - 1)
}


object Year {

    private val YearsInCycle = 19;


    private val LeapYears = List(3, 6, 8, 11, 14, 17, 19)


    private def isLeap(numberInCycle: Int) = LeapYears.contains(numberInCycle)


    private def lengthInMonths(numberInCycle: Int): Int = if (isLeap(numberInCycle)) MonthsInLeapYear else MonthsInNonLeapYear


    private val MonthsInNonLeapYear = 12


    private val MonthsInLeapYear = MonthsInNonLeapYear + 1


    private val MonthsBeforeYearInCycle = ((1 to YearsInCycle) map (lengthInMonths(_))).scanLeft (0)(_ + _)


    val MonthsInCycle = MonthsBeforeYearInCycle.last


    private val Adu = List(1, 4, 6)


    private def isAdu(day: Day) = Adu.contains(day.dayOfWeek)


    def apply(number: Int): Year = new Year(number)


    def apply(month: Month): Year = {
        val yearsBeforeCycle = (month.cycle - 1)*YearsInCycle
        val yearMonthIsInCycle = MonthsBeforeYearInCycle.count(_ < month.numberInCycle)
        Year(yearsBeforeCycle + yearMonthIsInCycle)
    }


    def apply(day: Day): Year = {
        val yearForSureBefore = (4 * day.number / (4 * 365 + 1)) - 1
        var result = Year(scala.math.max(1, yearForSureBefore))
        require(result.firstDay <= day.number)
        while (result.next.firstDay <= day.number) result = result.next
        result
    }
}
