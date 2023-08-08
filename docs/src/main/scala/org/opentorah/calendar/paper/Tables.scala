package org.opentorah.calendar.paper

import org.opentorah.astronomy.{Angles, Days2Rotation, MoonAnomalyMean, MoonHeadMean, MoonLongitudeMean, SunApogee, SunLongitudeMean}
import org.opentorah.metadata.Language
import org.opentorah.numbers.BigRational
import Angles.Rotation
import Rotation.Interval
import Days2Rotation.Days
import java.io.File

object Tables:

  private def printed(days2Rotation: Days2Rotation): Table[Days] =
    Table[Days](days2Rotation.all)(
      Table.Column("days"   , _.number),
      Table.Column("printed", days2Rotation.value)
    )

  private def printedAndCalculated(days2Rotation: Days2Rotation, calculate: Days => Option[Rotation]): Table[Days] =
    // TODO the value used for the reconstruction should become the table's title...
    Table[Days](days2Rotation.all)(
      Table.Column("days", _.number),
      Table.Column("printed", days2Rotation.value),
      Table.Column("calculated", (to: Days) => calculate(to)
        .getOrElse("")),
      Table.Column("difference", (to: Days) => calculate(to)
        .map(reconstructed => days2Rotation.value(to)-reconstructed)
        .getOrElse(""))
    )

  private def printedAndCalculatedForValue(
    days2Rotation: Days2Rotation,
    from: Rotation,
    roundUpTo: Option[Angles.Digit]
  ): Table[Days] =
    printedAndCalculated(days2Rotation, (to: Days) => Some(days2Rotation.calculate(
      from,
      to.number,
      roundUpTo.getOrElse(days2Rotation.precision(to)).position
    )))

  private def exactMin(days2Rotation: Days2Rotation): Table[Days] =
    val data: Map[Days, (Interval, Int)] = days2Rotation.exactMin
    Table[Days](Days.all)(
      Table.Column("days", _.number),
      Table.Column("precision", data(_)._2),
      Table.Column("from", data(_)._1.from),
      Table.Column("to", data(_)._1.to),
    )

  private def exact(data: Map[Days, Interval]): Table[Days] =
    Table[Days](Days.all)(
      Table.Column("days", _.number),
      Table.Column("from", data(_).from),
      Table.Column("to", data(_).to),
    )

  private def relationship(data: Map[Days, Interval], length: Int): Table[Days] =
    def mkColumn(to: Days): Table.Column[Days] = Table.Column[Days](
      heading = to.number.toString,
      (from: Days) => if from.number <= to.number then "" else
        Days2Rotation.IntervalRelationship.get(data(from), data(to), length) match
          case Days2Rotation.IntervalRelationship.Intersect(_) => "*"
          case Days2Rotation.IntervalRelationship.DoNotIntersect => "-"
          case Days2Rotation.IntervalRelationship.Separated(separator) => separator.toString
    )

    Table[Days](Seq(Days.Ten, Days.Month, Days.Hundred, Days.Year, Days.Thousand, Days.TenThousand))(
      Seq(Table.Column[Days]("days", _.number)) ++
      Seq(Days.One, Days.Ten, Days.Month, Days.Hundred, Days.Year, Days.Thousand).map(mkColumn)
    *)

  private def solarYearAndMovement: Table[SunLongitudeMean.Opinion] =
    def emphasize(opinion: SunLongitudeMean.Opinion, what: AnyRef, isVelocity: Boolean): String =
      val str: String = what.toString
      if isVelocity == opinion.isBasedOnVelocity then s"_${str}_" else str

    Table[SunLongitudeMean.Opinion](Seq(
      SunLongitudeMean.Opinion.Shmuel,
      SunLongitudeMean.Opinion.RavAda,
      SunLongitudeMean.Opinion.Almagest,
      SunLongitudeMean.Opinion.AlBattani.Pirush.FromYearLength,
      SunLongitudeMean.Opinion.AlBattani.Pirush.FromMovement,
      SunLongitudeMean.Opinion.AlBattani.Neugebauer,
      SunLongitudeMean.Opinion.Rambam
    ))(
      Table.Column("opinion", _.name),
      Table.Column("movement in one day", opinion => emphasize(
        opinion,
        opinion.movementInOneDay,
        isVelocity = true
      )),
      Table.Column("year length in days", opinion => emphasize(
        opinion,
        s"365 1/4 - ${BigRational(365) + BigRational(1,4) - opinion.yearLengthRational}",
        false
      )),
      Table.Column("year length", _.yearLength),
      Table.Column("reconstructs", opinion => Days.all.filter(opinion.isReconstructable).map(_.number).mkString(" "))
    )

  private def sunAtCreation: Table[SunLongitudeMean.SunAtCreation] = Table[SunLongitudeMean.SunAtCreation](Seq(
    SunLongitudeMean.SunAtCreation.Nisan,
    SunLongitudeMean.SunAtCreation.Tishrei
  ))(
    Table.Column("on", _.day.toLanguageString(using Language.English.toSpec)),
    Table.Column("season", _.season.name),
    Table.Column("at", _.position),
    Table.Column("from", _.velocity.from),
    Table.Column("to", _.velocity.to)
  )

  def main(args: Array[String]): Unit =
    val directory: File = File(if !args.isEmpty then args(0) else "/tmp/xxx/tables/")
    directory.mkdirs

    def writeTable(days2Rotation: Days2Rotation, suffix: String, table: Table[Days]): Unit =
      table.writeAsciidoc(directory, s"${days2Rotation.name}-$suffix")

    def writePrintedAndCalculated(
      days2Rotation: Days2Rotation,
      suffix: String,
      from: Rotation,
      roundUpTo: Option[Angles.Digit] = None
    ): Unit =
      writeTable(days2Rotation, suffix, printedAndCalculatedForValue(days2Rotation, from, roundUpTo))

    for days2Rotation: Days2Rotation <- List(SunLongitudeMean, SunApogee, MoonLongitudeMean, MoonHeadMean, MoonAnomalyMean) do
      def write(suffix: String, table: Table[Days]): Unit = writeTable(days2Rotation, suffix, table)

      write("printed", printed(days2Rotation))
      write("printedAndCalculated", printedAndCalculated(days2Rotation, days2Rotation.calculate))
      write("exactMin", exactMin(days2Rotation))
      val exactData: Map[Days, Interval] = days2Rotation.exact(days2Rotation.exactMinLength)
      write("exact", exact(exactData))
      write("compatibility", relationship(exactData, days2Rotation.exactMinLength))
      val exact6Data: Map[Days, Interval] = days2Rotation.exact(6)
      write("exact6", exact(exact6Data))
      write("compatibility6", relationship(exact6Data, 6))

    writePrintedAndCalculated(SunLongitudeMean, "printedAndCalculatedAlBattaniPirush", SunLongitudeMean.Opinion.AlBattani.Pirush.FromMovement.movementInOneDay)
    writePrintedAndCalculated(SunLongitudeMean, "printedAndCalculatedAlBattaniNeugebauer", SunLongitudeMean.Opinion.AlBattani.Neugebauer.movementInOneDay)
    writePrintedAndCalculated(SunLongitudeMean, "printedAndCalculatedRambam", SunLongitudeMean.Opinion.Rambam.movementInOneDay)
    solarYearAndMovement.writeAsciidoc(directory, "slm-opinions")
    sunAtCreation.writeAsciidoc(directory, "slm-at-creation")

    writePrintedAndCalculated(SunApogee, "printedAndCalculatedRambam", SunApogee.rambamValue)
    writePrintedAndCalculated(SunApogee, "printedAndCalculatedRambam3", SunApogee.rambamValue, Some(Angles.Digit.THIRDS))

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
//  // XXX do not turn what used to be a Map into a List and back :)
//  // Unfold sort()
//  private def tables[A,B](name: String, dataList: List[(A, B)], f: (Map[A, B]) => List[Columns[A]]) =
//    f(Map(dataList: _*)) map (_.tabulate(name, dataList map (_._1)))
//
//  private[this] def sort[A <: Ordered[A], B](map: Map[A, B]): List[(A, B)] = map.toList.sortWith((l, r) => (l._1 < r._1))
//  tables("mva", sort(MoonAnomalyVisible), mvaTables)
