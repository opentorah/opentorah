package org.opentorah.calendar.paper

import org.opentorah.angles.Angles.Rotation
import org.opentorah.astronomy.{SunLongitudeMean, Time2Rotation}
import java.io.File

object Tables {

  def printed(time2Rotation: Time2Rotation)(rows: Time2Rotation.Days*): Table = Table(rows: _*)(
    Table.Column("days" , days => days),
    Table.Column("printed", days => time2Rotation.value(days))
  )

  def reconstructed(time2Rotation: Time2Rotation)(rows: Time2Rotation.Days*): Table = {
    val from: Int = rows.head
    def reconstructed(days: Int): Rotation = (time2Rotation.value(from)*(days/from)).canonical
    def difference   (days: Int): Rotation = time2Rotation.value(days) - reconstructed(days)
    def blank(days: Int, value: Rotation): String = if (days == from) "" else value.toString

    Table(rows: _*)(
      Table.Column("days"         , days => days),
      Table.Column("printed"      , days => time2Rotation.value(days)),
      Table.Column("reconstructed", days => blank(days, reconstructed(days))),
      Table.Column("difference"   , days => blank(days, difference   (days)))
    )
  }

  def main(args: Array[String]): Unit = {
    val directory = new File(if (!args.isEmpty) args(0) else "/tmp/xxx/tables/")
    directory.mkdirs

    Tables.printed(SunLongitudeMean)(1, 10, 100, 1000, 10000, 29, 354).writeDocbook(directory, "slm-printed")
    Tables.reconstructed(SunLongitudeMean)(1, 10, 29, 100, 354, 1000, 10000).writeDocbook(directory, "slm-reconstructed-1")
    Tables.reconstructed(SunLongitudeMean)(10, 100, 1000, 10000).writeDocbook(directory, "slm-reconstructed-10")
    Tables.reconstructed(SunLongitudeMean)(100, 1000, 10000).writeDocbook(directory, "slm-reconstructed-100")
    Tables.reconstructed(SunLongitudeMean)(1000, 10000).writeDocbook(directory, "slm-reconstructed-1000")

//      Seq(Key.One, Key.Ten, Key.Hundred, Key.Thousand, Key.TenThousand))
  }


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
//
//
//  private[this] def sort[A <: Ordered[A], B](map: Map[A, B]): List[(A, B)] = map.toList.sortWith((l, r) => (l._1 < r._1))
//  tables("mva", sort(MoonAnomalyVisible), mvaTables)
}
