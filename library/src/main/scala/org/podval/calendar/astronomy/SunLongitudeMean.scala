package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.Rotation

object SunLongitudeMean extends Time2Rotation {
  // KH 12:1
  final override val one        : Rotation = Rotation(  0, 59,  8)
  final override val ten        : Rotation = Rotation(  9, 51, 23)
  final override val hundred    : Rotation = Rotation( 98, 33, 53)
  final override val thousand   : Rotation = Rotation(265, 38, 50) // remainder
  final override val tenThousand: Rotation = Rotation(136, 28, 20)

  // TODO reconstructs if calculated as 3*v(10)-v(1); not if using the exact value :)
  final override val month      : Rotation = Rotation( 28, 35,  1)
  final override val year       : Rotation = Rotation(348, 55, 15)  // TODO ??

  // Ibn Habib on Pirush gives Albatani value as:
  final val albataniValue: Rotation = Rotation(0, 59, 8, 20, 35)

  // Moznaim Rambam in English gives this value in KH 12:1 note 1 (without a reference) as the one
  // Rambam uses in his calculations.
  // TODO which of the year lengths does this correspond?
  final override val rambamValue: Rotation = Rotation(0, 59, 8, 19, 48)

  final override val almagestValue: Rotation = Rotation(0, 59, 8, 17, 13, 12, 31)
}
