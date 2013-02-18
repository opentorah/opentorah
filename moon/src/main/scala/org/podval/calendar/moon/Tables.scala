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

package org.podval.calendar.moon

import scala.math.{sin, cos, tan, round}


import java.io.File


object Tables {

    private def dayTables(data: Map[Int, Angle]) = {
        def exactify(days: Int) = Angle.exactify(data(1), days, data(days))
        def reconstruct(days: Int) = Angle.fromDegrees(exactify(days), 6)

        val days = new Column("days", "n", (days: Int) => days)
        val value = new Column("value", "v(n)", (days: Int) => data(days))
        val calculated = new Column("calculated", "v(1)*n", (days: Int) => data(1)*days)
        val reconstructed = new Column("reconstructed 1-day movement", "r(n)", reconstruct)
        val recalculated = new Column("recalculated", "r(10000)*n", (days: Int) => reconstruct(10000)*days)

        List(
            new Columns("original", days, value),
            new Columns("calculated", days, value, calculated),
            new Columns("reconstructed", days, value, reconstructed),
            new Columns("recalculated", days, value, recalculated)
        )
    }


    private def mvaTables(data: Map[Angle, Angle]) = {
        def e(a: Angle, v: Angle) = {
            val ar = a.toRadians
            val vr = v.toRadians

            val e = 1/tan(vr)*sin(ar)-cos(ar)

            round(e*100)/100.0
        }

        val mca = new Column("corrected anomaly", "mca", (a: Angle) => a)
        val mva = new Column("visible anomaly", "mva", (a: Angle) => data(a))
        val calculatee = new Column("e", "", (a: Angle) => e(a, data(a)))

        List(
            new Columns("original", mca, mva),
            new Columns("withe", mca, mva, calculatee)
        )
    }
    

    private def tables[A,B](name: String, dataList: List[(A, B)], f: (Map[A, B]) => List[Columns[A]]) =
        f(Map(dataList: _*)) map (_.tabulate(name, dataList map (_._1)))


    private def allTables =
        tables("sml", TablesData.SunMeanLongitude, dayTables) ++
        tables("mml", TablesData.MoonMeanLongitude, dayTables) ++
        tables("mma", TablesData.MoonMeanAnomaly, dayTables) ++
        tables("mva", sort(VisibleAnomaly.CORRECT), mvaTables)


    private[this] def sort[A <: Ordered[A], B](map: Map[A, B]): List[(A, B)] = map.toList.sortWith((l, r) => (l._1 < r._1))


    def main(args: Array[String]) {
        val directory = new File(if (!args.isEmpty) args(0) else "/tmp/xxx/tables/")
        directory.mkdirs
        allTables foreach (_.write(directory))
    }
}
