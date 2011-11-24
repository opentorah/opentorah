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

class MonthG private (number: Int) extends Numbered[Month](number) {

    def year: YearG = YearG(this)


    def numberInYear: Int = number - year.firstMonth + 1


    def day(day: Int): DayG = {
        require (0 < day && day <= length)
        DayG(firstDay + day - 1)
    }


    def firstDay: Int = year.firstDay + descriptor.daysBefore


    def name: MonthG.Name.MonthName = descriptor.name


    def length: Int = descriptor.length


    private def descriptor = year.months(numberInYear - 1)
}


object MonthG {

    object Name extends Enumeration {

        type MonthName = Value 


        val January, February, March, April, May, June,
            July, August, September, October, November, December = Value
    }


    final class Descriptor(val name: Name.MonthName, val length: Int, val daysBefore: Int)


    def apply(number: Int): MonthG = new MonthG(number)
}
