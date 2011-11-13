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


final class Month(val number: Int) {

    require(0 < number)


    override def equals(other: Any): Boolean = other match {
        case that: Month => (number == that.number)
        case _ => false
    }


    override def hashCode = number


    def cycle: Int = (number / Year.MonthsInCycle) + 1


    def numberInCycle: Int = number % Year.MonthsInCycle


    lazy val year: Year = Year(cycle, Year.yearMonthIsInCycle(numberInCycle))


    def numberInYear: Int = numberInCycle - year.monthsBeforeInCycle


    def newMoon: Moment = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1) 


    def name: Month.Name.Type = {
        val n = numberInYear-1
        if (!year.isLeap) Month.Name(n) else {
            if (n > Month.Name.Nisan.id) Month.Name(n-1) else
            if (n == Month.Name.Adar.id) Month.Name.AdarI else
            if (n == Month.Name.Nisan.id) Month.Name.AdarII else
                Month.Name(n)
        }
    }


    // KH 8:5,6
    def length: Int = {
        val name = this.name

        if (name == Month.Name.Marheshvan) {
            year.kind match {
            case Year.Kind.Short => 29 
            case Year.Kind.Regular => 29 
            case Year.Kind.Full => 30 
            }
        } else
        if (name == Month.Name.Kislev) {
            year.kind match {
            case Year.Kind.Short => 29 
            case Year.Kind.Regular => 30 
            case Year.Kind.Full => 30 
            }
        } else
            Month.lengths(name)
    }
}


object Month {

    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    val MeanLunarPeriod = Moment(29, 12, 793)


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    val FirstNewMoon = Moment(Day(2), Time.ofNight(5, 204))


    object Name extends Enumeration {

        type Type = Value 


        val Tishrei, Marheshvan, Kislev, Teves, Shvat, Adar,
            Nisan, Iyar, Sivan, Tammuz, Av, Elul, 
            AdarI, AdarII = Value
    }


    private val lengths = Map(
        Name.Tishrei -> 30,
        Name.Teves -> 29,
        Name.Shvat -> 30,
        Name.Adar -> 29,
        Name.AdarI -> 30,
        Name.AdarII -> 29,
        Name.Nisan -> 30,
        Name.Iyar -> 29,
        Name.Sivan -> 30,
        Name.Tammuz -> 29,
        Name.Av -> 30,
        Name.Elul -> 29
    )


    def apply(number: Int): Month = new Month(number)


    def apply(year: Year, month: Int): Month = Month(year.monthsBefore + month)


    def main(args: Array[String]) {
        println(Year(   2).month(1).newMoon.toMinutesString)
        println(Year(5772).month(2).newMoon.toMinutesString)
        println(Year(5772).month(3).newMoon.toMinutesString)
        println(Year(5772).month(4).newMoon.toMinutesString)
    }
}
