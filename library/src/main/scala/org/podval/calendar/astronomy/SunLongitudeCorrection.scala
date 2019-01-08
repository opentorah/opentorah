package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.Rotation

// TODO there are some diagrams in the Moznaim Rambam at this point.
object SunLongitudeCorrection  {
  final val table: InterpolatedTable[Rotation] = new InterpolatedTable[Rotation] {
    // KH 13:4
    final override val values: Map[Rotation, Rotation] = Map(
      row(  0, 0,  0),
      row( 10, 0, 20),
      row( 20, 0, 40),
      row( 30, 0, 58),
      row( 40, 1, 15),
      row( 50, 1, 29),
      row( 60, 1, 41),
      row( 70, 1, 51),
      row( 80, 1, 57),
      row( 90, 1, 59),
      row(100, 1, 58),
      row(110, 1, 53),
      row(120, 1, 45),
      row(130, 1, 33),
      row(140, 1, 19),
      row(150, 1,  1),
      row(160, 0, 42),
      row(170, 0, 21),
      row(180, 0,  0)
    )

    // KH 13:2-3
    final override def calculate(sunCourse: Rotation): Rotation = {
      val angle: Rotation = sunCourse.canonical
      if (angle <= Rotation(180)) -interpolate(angle) else interpolate(Rotation(360) - angle)
    }
  }

  private def row(argumentDegrees: Int, valueDegrees: Int, valueMinutes: Int): (Rotation, Rotation) =
    Rotation(argumentDegrees) -> Rotation(valueDegrees, valueMinutes)
}
