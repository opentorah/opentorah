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


final class YearG private(number: Int) extends Numbered[YearG](number) {

    // TODO give names to constants?
    def isLeap: Boolean = (number % 4 == 0) && ((number % 100 != 0) || (number % 400 == 0))


    def firstDay: Int =
        YearG.DaysInNonLeapYear * (number - 1) + (number - 1)/4 - (number - 1)/100 + (number - 1)/400 + 1


    def lengthInMonths = YearG.MonthsInYear


    def firstMonth: Int = YearG.MonthsInYear*(number - 1) + 1


    def lengthInDays = if (isLeap) YearG.DaysInLeapYear else YearG.DaysInNonLeapYear


    def month(numberInYear: Int): MonthG = {
        require(0 < numberInYear && numberInYear <= lengthInMonths)
        MonthG(firstMonth + numberInYear - 1)
    }


    def month(name: MonthG.Name.MonthName): MonthG = month(months.indexWhere(_.name == name) + 1)


    def month(day: DayG): MonthG = {
        require(0 < day.dayOfYear && day.dayOfYear <= lengthInDays)
        month(months.count(_.daysBefore < day.dayOfYear))
    }


    def months: List[MonthG.Descriptor] = YearG.months(this)


    def next: YearG = YearG(number + 1)


    def prev: YearG = YearG(number - 1)
}


object YearG {

    val MonthsInYear = 12


    val DaysInNonLeapYear = 365


    val DaysInLeapYear = DaysInNonLeapYear + 1


    def months(year: YearG): List[MonthG.Descriptor] = Months(year.isLeap)


    private val Months: Map[Boolean, List[MonthG.Descriptor]] =
        Map(List(true, false).map(isLeap => isLeap -> months(isLeap)): _*)

    
    private def months(isLeap: Boolean): List[MonthG.Descriptor] = {
        val namesAndLengths = this.namesAndLengths(isLeap)
        val (names, lengths) = namesAndLengths.unzip
        val daysBefore = lengths.scanLeft(0)(_ + _).init
        (namesAndLengths zip daysBefore) map (m => new MonthG.Descriptor(m._1._1, m._1._2, m._2))
    }


    private def namesAndLengths(isLeap: Boolean) = {
        import MonthG.Name._

        List(
            (January, 31),
            (February, if (isLeap) 29 else 28),
            (March, 31),
            (April, 30),
            (May, 31),
            (June, 30),
            (July, 31),
            (August, 31),
            (September, 30),
            (October, 31),
            (November, 30),
            (December, 31)
        )
    }


    def apply(number: Int): YearG = new YearG(number)


    def apply(month: MonthG): YearG = YearG((month.number - 1) / MonthsInYear +1)


    def apply(day: DayG): YearG = {
        // TODO give names to constants
        val yearForSureNotAfter = (4 * day.number / (4 * 365 + 1)) - 1
        var result = YearG(yearForSureNotAfter) // Year(scala.math.max(1, yearForSureNotAfter))
        require(result.firstDay <= day.number)
        while (result.next.firstDay <= day.number) result = result.next
        result
    }
}
