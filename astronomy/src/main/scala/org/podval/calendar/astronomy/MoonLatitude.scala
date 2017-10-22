package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

object MoonLatitude extends Angle2Angle {
  final override val table: Table = new Table {
    // KH 16:11
    override val values: Map[Angle, Angle] = Map(
      Angle( 0) -> Angle(0,  0),
      Angle(10) -> Angle(0, 52),
      Angle(20) -> Angle(1, 43),
      Angle(30) -> Angle(2, 30),
      Angle(40) -> Angle(3, 13),
      Angle(50) -> Angle(3, 50),
      Angle(60) -> Angle(4, 20),
      Angle(70) -> Angle(4, 42),
      Angle(80) -> Angle(4, 55),
      Angle(90) -> Angle(5,  0)
    )

    // KH 16:13-15
    override def calculate(moonLatitudeCourse: Angle): Angle = {
      val angle: Angle = moonLatitudeCourse.canonical
      // canonical angle is always >= Angle(0)
      if (angle <= Angle( 90)) interpolateNg(angle           ) else // KH 16:11
      if (angle <= Angle(180)) interpolateNg(Angle(180)-angle) else // KH 16:13
      if (angle <= Angle(270)) interpolateNg(angle-Angle(180)) else // KH 16:14
                               interpolateNg(Angle(360)-angle)      // KH 16:15
    }
  }
}
