package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

// TODO there are some diagrams in the Moznaim Rambam at this point.
object SunLongitudeCorrection  {
  final val table: InterpolatedTable = new InterpolatedTable {
    // KH 13:4
    final override val values: Map[Angle, Angle] = Map(
      Angle(  0) -> Angle(0    ),
      Angle( 10) -> Angle(0, 20),
      Angle( 20) -> Angle(0, 40),
      Angle( 30) -> Angle(0, 58),
      Angle( 40) -> Angle(1, 15),
      Angle( 50) -> Angle(1, 29),
      Angle( 60) -> Angle(1, 41),
      Angle( 70) -> Angle(1, 51),
      Angle( 80) -> Angle(1, 57),
      Angle( 90) -> Angle(1, 59),
      Angle(100) -> Angle(1, 58),
      Angle(110) -> Angle(1, 53),
      Angle(120) -> Angle(1, 45),
      Angle(130) -> Angle(1, 33),
      Angle(140) -> Angle(1, 19),
      Angle(150) -> Angle(1,  1),
      Angle(160) -> Angle(0, 42),
      Angle(170) -> Angle(0, 21),
      Angle(180) -> Angle(0    )
    )

    // KH 13:2-3
    final override def calculate(sunCourse: Angle): Angle = {
      val angle: Angle = sunCourse.canonical
      if (angle <= Angle(180)) -interpolate(angle) else interpolate(Angle(360) - angle)
    }
  }
}
