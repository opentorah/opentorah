/*
 * Copyright 2011-2018 Podval Group.
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

package org.podval.calendar.paper

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.{SunLongitudeMean, MoonAnomalyMean, MoonAnomalyVisible, MoonLongitudeMean}

import scala.math.{sin, cos, tan, round}

import java.io.File


object Tables {

// TODO update to the cleaned up astronomy code.
//  private def dayTables(name: String, data: DayData) = {
//
//    val tables = {
//      val days = Column[Int]("days", "n", (days: Int) => days)
//      val value = Column("value", "v(n)", data.value)
//      val calculated = Column("calculated", "v(1)*n", data.calculated)
//      val reconstructed = Column("reconstructed 1-day movement", "r(n)", data.reconstructed)
//      val recalculated = Column("recalculated", "r(10000)*n", data.recalculated)
//      val recalculated10 = Column("recalculated", "r(10)*n", data.recalculated10)
//      val almagest = Column("Alamgest", "*n", data.almagest)
//
//      List(
//        new Columns("original", days, value),
//        new Columns("calculated", days, value, calculated),
//        new Columns("reconstructed", days, value, reconstructed),
//        new Columns("recalculated", days, value, recalculated),
//        new Columns("recalculated10", days, value, recalculated10),
//        new Columns("almagest", days, value, almagest)
//      )
//    }
//
//    tables map (_.tabulate(name, data.keys))
//  }
//
//
//  private def mvaTables(data: Map[Angle, Angle]) = {
//      def e(a: Angle, v: Angle) = {
//          val ar = a.toRadians
//          val vr = v.toRadians
//
//          val e = 1/tan(vr)*sin(ar)-cos(ar)
//
//          round(e*100)/100.0
//      }
//
//      val mca = Column("corrected anomaly", "mca", (a: Angle) => a)
//      val mva = Column("visible anomaly", "mva", (a: Angle) => data(a))
//      val calculatee = Column("e", "", (a: Angle) => e(a, data(a)))
//
//      List(
//          new Columns("original", mca, mva),
//          new Columns("withe", mca, mva, calculatee)
//      )
//  }
//
//
//  // XXX do not turn what used to be a Amp into a List and back :)
//  // Unfold sort()
//  private def tables[A,B](name: String, dataList: List[(A, B)], f: (Map[A, B]) => List[Columns[A]]) =
//    f(Map(dataList: _*)) map (_.tabulate(name, dataList map (_._1)))
//
//
//  private def allTables =
//    dayTables("sml", SunLongitudeMean.table) ++
//    dayTables("mml", MoonLongitudeMean.table) ++
//    dayTables("mma", MoonAnomalyMean.table) ++
//    tables("mva", sort(MoonAnomalyVisible.table), mvaTables)
//
//
//  private[this] def sort[A <: Ordered[A], B](map: Map[A, B]): List[(A, B)] = map.toList.sortWith((l, r) => (l._1 < r._1))
//
//
  def main(args: Array[String]): Unit = {
    val directory = new File(if (!args.isEmpty) args(0) else "/tmp/xxx/tables/")
    directory.mkdirs
//    allTables foreach (_.write(directory))
  }
}
