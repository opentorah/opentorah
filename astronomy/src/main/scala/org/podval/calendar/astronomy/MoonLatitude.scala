package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Rotation

object MoonLatitude {
  final val table: InterpolatedTable = new InterpolatedTable {
    // KH 16:11
    final override val values: Map[Rotation, Rotation] = Map(
      Rotation( 0) -> Rotation(0    ),
      Rotation(10) -> Rotation(0, 52),
      Rotation(20) -> Rotation(1, 43),
      Rotation(30) -> Rotation(2, 30),
      Rotation(40) -> Rotation(3, 13),
      Rotation(50) -> Rotation(3, 50),
      Rotation(60) -> Rotation(4, 20),
      Rotation(70) -> Rotation(4, 42),
      Rotation(80) -> Rotation(4, 55),
      Rotation(90) -> Rotation(5,  0)
    )

    // KH 16:13-15
    final override def calculate(moonLatitudeCourse: Rotation): Rotation = {
      val angle: Rotation = moonLatitudeCourse.canonical
      // canonical angle is always >= Angle(0)
      if (angle <= Rotation( 90)) interpolate(angle             ) else // KH 16:11
      if (angle <= Rotation(180)) interpolate(Rotation(180) - angle) else // KH 16:13
      if (angle <= Rotation(270)) interpolate(angle - Rotation(180)) else // KH 16:14
                               interpolate(Rotation(360) - angle)      // KH 16:15
    }
  }
}
