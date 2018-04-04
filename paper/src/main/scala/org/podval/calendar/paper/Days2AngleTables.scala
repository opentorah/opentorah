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

import org.podval.calendar.astronomy.{Days2Angle, SunLongitudeMean, SunApogee, MoonAnomalyMean, MoonHeadMean,
  MoonLongitudeMean}
import Days2Angle.Key

import java.io.File


object Days2AngleTables {

  def get: Seq[Table[Key]] = Seq(
    "slm" -> SunLongitudeMean,
    "sap" -> SunApogee,
    "mlm" -> MoonLongitudeMean,
    "mam" -> MoonAnomalyMean,
    "mhm" -> MoonHeadMean
  ).flatMap { case (name: String, data: Days2Angle) =>
    dayTables(name, data).map(_.tabulate(name, Key.all))
  }

  private def dayTables(name: String, data: Days2Angle): List[Columns[Key]] = {
    val days = Column[Key]("days", "n", _.number)
    val value = Column[Key]("value", "v(n)", data.value)
    val calculated = Column[Key]("calculated", "v(1)*n", data.calculated)
    val almagest = Column[Key]("Alamgest", "*n", data.calculatedAlmagest)
    //      val reconstructed = Column("reconstructed 1-day movement", "r(n)", data.reconstructed)
    //      val recalculated = Column("recalculated", "r(10000)*n", data.recalculated)
    //      val recalculated10 = Column("recalculated", "r(10)*n", data.recalculated10)

    List[Columns[Key]](
      new Columns[Key]("original", days, value),
      new Columns("calculated", days, value, calculated),
      new Columns("almagest", days, value, almagest) //,
      //        new Columns("reconstructed", days, value, reconstructed),
      //        new Columns("recalculated", days, value, recalculated),
      //        new Columns("recalculated10", days, value, recalculated10),
    )
  }
}
