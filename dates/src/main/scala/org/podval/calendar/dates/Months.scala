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

import MonthName._
import YearKind._


object Months {

    def name(month: Month): MonthName = months(month.year)(month.numberInYear-1).name


    // KH 8:5,6
    def length(month: Month): Int =  months(month.year)(month.numberInYear-1).length


    def numberInYear(year: Year, name: MonthName): Int = months(year).indexWhere(_.name == name) + 1


    private final class Descriptor(val name: MonthName, val length: Int)


    private object Descriptor {

        def apply(name: MonthName, length: Int) = new Descriptor(name, length)
    }


    private val months: Map[(YearKind, Boolean), List[Descriptor]] = Map(pairs: _*)


    private def pairs = for (isLeap <- List(true, false); kind <- YearKind.values) yield ((kind, isLeap) -> mkMonths(kind, isLeap))


    private def mkMonths(kind: YearKind, isLeap: Boolean): List[Descriptor] =
        List(
            Descriptor(Tishrei, 30),
            Descriptor(Marheshvan, if (kind == Full) 30 else 29),
            Descriptor(Kislev, if (kind == Short) 29 else 30),
            Descriptor(Teves, 29),
            Descriptor(Shvat, 30)
        ) ++
        (if (!isLeap)
            List(Descriptor(Adar, 29)) else
            List(Descriptor(AdarI, 30), Descriptor(AdarII, 30))
        ) ++
        List(
            Descriptor(Nisan, 30),
            Descriptor(Iyar, 29),
            Descriptor(Sivan, 30),
            Descriptor(Tammuz, 29),
            Descriptor(Av, 30),
            Descriptor(Elul, 29)
        )


    private def months(year: Year): List[Descriptor] = months((year.kind, year.isLeap))
}
