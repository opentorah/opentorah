package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import Days2Angle.Table

object SunApogee extends Days2Angle {
  // KH 12:2
  final override val table = new Table {
    final override val one        : Angle = Angle.zero // Rambam doesn't give this value
    final override val ten        : Angle = Angle(0,  0,  1, 30)
    final override val hundred    : Angle = Angle(0,  0, 15)
    final override val thousand   : Angle = Angle(0,  2, 30)
    final override val tenThousand: Angle = Angle(0, 25)

    final override val month      : Angle = Angle(0,  0,  4) // TODO: veod!
    final override val year       : Angle = Angle(0,  0, 53)
  }

  // KH 12:2
  final override val atEpoch: AnglePoint = Zodiac.Gemini.at(Angle(26, 45, 8))

  final override val rambamValue = Angle(0) // TODO

  final override val almagestValue = Angle(0) // TODO
}
