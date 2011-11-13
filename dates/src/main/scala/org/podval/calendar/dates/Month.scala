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


final class Month(val year: Year, val month: Int) {

    def number: Int = year.monthsBefore + month


    def newMoon: Time = Month.FirstNewMoon + Month.MeanLunarPeriod*(number-1) 
}


object Month {
    // Mean lunar period: 29 days 12 hours 793 parts (KH 6:3 )
    val MeanLunarPeriod = Time(29, 12, 793)


    // Molad of the year of Creation:
    // BeHaRaD: 5 hours 204 parts at night of the second day of Creation (KH 6:8)
    val FirstNewMoon = Time(1, 5, 204)


    def apply(number: Int): Month = {
        val cycle = (number / Year.MonthsInCycle) + 1
        val numberInCycle = number % Year.MonthsInCycle
        val year = Year(cycle, Year.yearMonthIsInCycle(numberInCycle))
        year.month(numberInCycle - year.monthsBeforeInCycle)
    }


    def apply(year: Year, month: Int): Month = new Month(year, month)


    def main(args: Array[String]) {
        println(Year(   2).month(1).newMoon.toMinutesString)
        println(Year(5772).month(2).newMoon.toMinutesString)
        println(Year(5772).month(3).newMoon.toMinutesString)
        println(Year(5772).month(4).newMoon.toMinutesString)
    }
}
