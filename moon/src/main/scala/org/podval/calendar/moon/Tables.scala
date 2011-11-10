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
            new PreTable("original", days, value),
            new PreTable("calculated", days, value, calculated),
            new PreTable("reconstructed", days, value, reconstructed),
            new PreTable("recalculated", days, value, recalculated)
        )
    }


    private def mvaTables(data: Map[Angle, Angle]) = {
        val cma = new Column("corrected anomaly", "cma", (a: Angle) => a)
        val vma = new Column("visible anomaly", "vma", (a: Angle) => data(a))

        List(
            new PreTable("original", cma, vma)
        )
    }
    

    private def tables[A,B](name: String, dataList: List[(A, B)], f: (Map[A, B]) => List[PreTable[A]]) =
        f(Map(dataList: _*)) map (_.toTable(name, dataList map (_._1)))


    private def allTables = 
        tables("mml", TablesData.MoonMeanLongitude, dayTables) ++
        tables("mma", TablesData.MoonMeanAnomaly, dayTables) ++
        tables("mva", TablesData.MoonVisibleAnomaly, mvaTables)


    def main(args: Array[String]) {
        val directory = new File(if (!args.isEmpty) args(0) else "/tmp/xxx/tables/")
        directory.mkdir
        allTables foreach (_.write(directory))
    }
}
