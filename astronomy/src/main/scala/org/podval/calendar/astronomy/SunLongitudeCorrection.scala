package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Rotation

// TODO there are some diagrams in the Moznaim Rambam at this point.
object SunLongitudeCorrection  {
  final val table: InterpolatedTable = new InterpolatedTable {
    // KH 13:4
    final override val values: Map[Rotation, Rotation] = Map(
      Rotation(  0) -> Rotation(0    ),
      Rotation( 10) -> Rotation(0, 20),
      Rotation( 20) -> Rotation(0, 40),
      Rotation( 30) -> Rotation(0, 58),
      Rotation( 40) -> Rotation(1, 15),
      Rotation( 50) -> Rotation(1, 29),
      Rotation( 60) -> Rotation(1, 41),
      Rotation( 70) -> Rotation(1, 51),
      Rotation( 80) -> Rotation(1, 57),
      Rotation( 90) -> Rotation(1, 59),
      Rotation(100) -> Rotation(1, 58),
      Rotation(110) -> Rotation(1, 53),
      Rotation(120) -> Rotation(1, 45),
      Rotation(130) -> Rotation(1, 33),
      Rotation(140) -> Rotation(1, 19),
      Rotation(150) -> Rotation(1,  1),
      Rotation(160) -> Rotation(0, 42),
      Rotation(170) -> Rotation(0, 21),
      Rotation(180) -> Rotation(0    )
    )

    // KH 13:2-3
    final override def calculate(sunCourse: Rotation): Rotation = {
      val angle: Rotation = sunCourse.canonical
      if (angle <= Rotation(180)) -interpolate(angle) else interpolate(Rotation(360) - angle)
    }
  }
}
