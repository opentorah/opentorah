package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

object MoonLongitudeDoubleElongationCorrection {
  // KH 15:2-3
  // TODO Moznaim Rambam, KH 15:2f3: discrepancy in the limits
  // TODO encode the limits
  def calculate(doubleElongation: Angle): Angle = {
    def between(from: Int, to: Int): Boolean =
      (Angle(from) <= doubleElongation) && (doubleElongation < Angle(to + 1))

    val result: Int =
      if (between(0, 5)) 0
      else if (between( 6, 11)) 1
      else if (between(12, 18)) 2
      else if (between(19, 24)) 3
      else if (between(25, 31)) 4
      else if (between(32, 38)) 5
      else if (between(39, 45)) 6
      else if (between(46, 51)) 7
      else if (between(52, 59)) 8
      else if (between(60, 63)) 9
      else 0 // so that Calculator doesn't throw exceptions

    Angle(result)
  }
}
