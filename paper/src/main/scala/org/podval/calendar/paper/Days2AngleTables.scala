package org.podval.calendar.paper

import org.podval.calendar.astronomy.{Time2Rotation, SunLongitudeMean, SunApogee, MoonAnomalyMean, MoonHeadMean,
  MoonLongitudeMean}
import Time2Rotation.Key

object Days2AngleTables {

  def get: Seq[Table[Key]] = Seq(
    "slm" -> SunLongitudeMean,
    "sap" -> SunApogee,
    "mlm" -> MoonLongitudeMean,
    "mam" -> MoonAnomalyMean,
    "mhm" -> MoonHeadMean
  ).flatMap { case (name: String, data: Time2Rotation) =>
    dayTables(name, data).map(_.tabulate(name, Key.values))
  }

  private def dayTables(name: String, data: Time2Rotation): List[Columns[Key]] = {
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
