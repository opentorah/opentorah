package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

object MoonLatitude {
  final val table: InterpolatedTable = new InterpolatedTable {
    // KH 16:11
    final override val values: Map[Angle, Angle] = Map(
      Angle( 0) -> Angle(0    ),
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
    final override def calculate(moonLatitudeCourse: Angle): Angle = {
      val angle: Angle = moonLatitudeCourse.canonical
      // canonical angle is always >= Angle(0)
      if (angle <= Angle( 90)) interpolate(angle             ) else // KH 16:11
      if (angle <= Angle(180)) interpolate(Angle(180) - angle) else // KH 16:13
      if (angle <= Angle(270)) interpolate(angle - Angle(180)) else // KH 16:14
                               interpolate(Angle(360) - angle)      // KH 16:15
    }
  }
}
