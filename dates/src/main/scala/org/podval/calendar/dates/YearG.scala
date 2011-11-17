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


final class YearG private(number: Int) extends Numbered[YearG](number) with Ordered[YearG] {

    def isLeap: Boolean = (number % 4 == 0) && ((number % 100 != 0) || (number % 400 == 0))


    def lengthInMonths = 12


    def lengthInDays = if (isLeap) YearG.DaysInLeapYear else YearG.DaysInNonLeapYear


    def month(numberInYear: Int): MonthG = {
        require(0 < numberInYear && numberInYear <= lengthInMonths)
        MonthG(firstMonth + numberInYear - 1)
    }


    def month(name: MonthG.Name.MonthName): MonthG = month(months.indexWhere(_.name == name) + 1)


    def month(day: DayG): Month = {
        require(0 < day.dayOfYear && day.dayOfYear <= lengthInDays)
        month(months.count(_.daysBefore < day.dayOfYear))
    }

//
//
//

//    @Override
//    protected int daysInYearsBeforeYear(final int year) {
//        final int y = year-1;
//        return 365 * y + y/4 - y/100 + y/400;
//    }
}


object YearG {

    val DaysInNonLeapYear = 365


    val DaysInLeapYear = DaysInNonLeapYear + 1


    def months(year: YearG): List[MonthG.Descriptor] = Months(year.isLeap)


    private val Months: Map[Boolean, List[MonthG.Descriptor]] =
        Map(List(true, false).map(isLeap => months(isLeap)))

    

    private def months(kind: Kind.YearKind, isLeap: Boolean): List[Month.Descriptor] = {
        val namesAndLengths = this.namesAndLengths(kind, isLeap)
        val (names, lengths) = namesAndLengths unzip
        val daysBefore = lengths.scanLeft(0)(_ + _).init
        (namesAndLengths zip daysBefore) map (m => new Month.Descriptor(m._1._1, m._1._2, m._2))
    }


    private def namesAndLengths(kind: Kind.YearKind, isLeap: Boolean) = {
        import Kind._
        import Month.Name._

        List(
            (Tishrei, 30),
            (Marheshvan, if (kind == Full) 30 else 29),
            (Kislev, if (kind == Short) 29 else 30),
            (Teves, 29),
            (Shvat, 30)
        ) ++
        (if (!isLeap)
            List((Adar, 29)) else
            List((AdarI, 30), (AdarII, 30))
        ) ++
        List(
            (Nisan, 30),
            (Iyar, 29),
            (Sivan, 30),
            (Tammuz, 29),
            (Av, 30),
            (Elul, 29)
        )
    }
//(January, 31)
//(February, 28) (February, 29)
//(March", 31)
//(April", 30)
//(May", 31)
//(June", 30)
//(July", 31)
//(August", 31)
//(September", 30)
//(October", 31)
//(November", 30)
//(December", 31)
}
