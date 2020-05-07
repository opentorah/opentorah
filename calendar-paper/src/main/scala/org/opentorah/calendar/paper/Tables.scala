package org.opentorah.calendar.paper

import java.io.File
import org.opentorah.astronomy.{SunLongitudeMean, Time2Rotation}
import Time2Rotation.Key

object Tables {

  def main(args: Array[String]): Unit = {
    val directory = new File(if (!args.isEmpty) args(0) else "/tmp/xxx/tables/")
    directory.mkdirs

    velocityOriginal(SunLongitudeMean).write(directory, "slm-original",
      Seq(Key.One, Key.Ten, Key.Hundred, Key.Thousand, Key.TenThousand))
  }

  private def velocityOriginal(data: Time2Rotation): Table[Key] = {
    val days = Column[Key]("days", "n", _.number)
    val value = Column[Key]("value", "v(n)", data.value)
    val calculated = Column[Key]("calculated", "v(1)*n",
      (key: Key) => (data.one*key.number).canonical)

    new Table(days, value, calculated)
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
