package org.opentorah.calendar.paper

import org.opentorah.astronomy.Angles.Rotation
import org.opentorah.astronomy.{MoonAnomalyMean, MoonHeadMean, MoonLongitudeMean, SunApogee, SunLongitudeMean, Days2Rotation}
import Days2Rotation.Days
import java.io.File

object Tables:

  private def printed(time2Rotation: Days2Rotation): Table[Days] =
    Table[Days](time2Rotation.all)(
      Table.Column("days"   , _.number),
      Table.Column("printed", time2Rotation.value)
    )

  private def nonReconstructable(time2Rotation: Days2Rotation): Table[Days] =
    val reconstructed = time2Rotation.reconstructed

    def mkColumn(to: Days): Table.Column[Days] = Table.Column[Days](
      to.number.toString,
      from => reconstructed(from)(to) match
        case None => " "
        case Some(signum) => if signum > 0 then "+" else if signum < 0 then "-" else "="
    )

    Table[Days](time2Rotation.reconstructFrom)(
      Table.Column("from", _.number),
      mkColumn(Days.Ten),
      mkColumn(Days.Month),
      mkColumn(Days.Hundred),
      mkColumn(Days.Year),
      mkColumn(Days.Thousand),
      mkColumn(Days.TenThousand),
    )

  private def exactified(time2Rotation: Days2Rotation, maxLength: Int): Table[Days] =
    val data = time2Rotation.exactified(maxLength)
    Table[Days](time2Rotation.exactify)(
      Table.Column("days", _.number),
      Table.Column("printed", time2Rotation.value),
      Table.Column("min", data(_)._1),
      Table.Column("length=min", data(_)._2),
      Table.Column(s"length=$maxLength", data(_)._3)
    )

  def main(args: Array[String]): Unit =
    val directory: File = File(if !args.isEmpty then args(0) else "/tmp/xxx/tables/")
    directory.mkdirs

    for time2rotation <- List(SunLongitudeMean, SunApogee, MoonLongitudeMean, MoonHeadMean, MoonAnomalyMean) do
//      printed(time2rotation).writeMarkdown(directory, s"${time2rotation.name}-printed")
//      nonReconstructable(time2rotation).writeMarkdown(directory, s"${time2rotation.name}-reconstructed")
//      exactified(time2rotation, 6).writeMarkdown(directory, s"${time2rotation.name}-exactified")
      printed(time2rotation).writeDocbook(directory, s"${time2rotation.name}-printed")
      nonReconstructable(time2rotation).writeDocbook(directory, s"${time2rotation.name}-reconstructed")
      exactified(time2rotation, 6).writeDocbook(directory, s"${time2rotation.name}-exactified")

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
