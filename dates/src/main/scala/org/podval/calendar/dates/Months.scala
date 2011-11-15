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

    final class Descriptor(val name: MonthName, val length: Int, val daysBefore: Int)


    def months(year: Year): List[Descriptor] = Months(year.isLeap)(year.kind)


    private val Months: Map[Boolean, Map[YearKind, List[Descriptor]]] =
        Map(List(true, false).map(isLeap =>
            isLeap -> Map(YearKind.values.toSeq.map(kind => kind -> mkMonths(kind, isLeap)): _*)
        ): _*)


    private def mkMonths(kind: YearKind, isLeap: Boolean): List[Descriptor] = {
        val namesAndLengths = this.namesAndLengths(kind, isLeap)
        val (names, lengths) = namesAndLengths unzip
        val daysBefore = lengths.scanLeft(0)(_ + _).init
        val x = namesAndLengths zip daysBefore
        x map (m => new Descriptor(m._1._1, m._1._2, m._2))
    }


    private def namesAndLengths(kind: YearKind, isLeap: Boolean) =
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
