package org.opentorah.astronomy

import org.opentorah.angles.Angles.Rotation

object MoonLatitude {
  final val table: InterpolatedTable[Rotation] = new InterpolatedTable[Rotation] {
    // KH 16:11
    final override val values: Map[Rotation, Rotation] = Map(
      row( 0, 0,  0),
      row(10, 0, 52),
      row(20, 1, 43),
      row(30, 2, 30),
      row(40, 3, 13),
      row(50, 3, 50),
      row(60, 4, 20),
      row(70, 4, 42),
      row(80, 4, 55),
      row(90, 5,  0)
    )

    // KH 16:13-15
    final override def calculate(moonLatitudeCourse: Rotation): Rotation = {
      def forCanonical(argument: Rotation): Rotation = interpolate(argument.canonical)
      val angle: Rotation = moonLatitudeCourse.canonical
      // canonical angle is always >= Rotation(0)
      if (angle <= Rotation( 90)) forCanonical(angle                ) else // KH 16:11
      if (angle <= Rotation(180)) forCanonical(Rotation(180) - angle) else // KH 16:13
      if (angle <= Rotation(270)) forCanonical(angle - Rotation(180)) else // KH 16:14
                                  forCanonical(Rotation(360) - angle)      // KH 16:15
    }
  }

  private def row(argumentDegrees: Int, valueDegrees: Int, valueMinutes: Int): (Rotation, Rotation) =
    Rotation(argumentDegrees) -> Rotation(valueDegrees, valueMinutes)
}
