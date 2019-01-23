package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.Rotation

object MoonLongitudeMean extends Time2Rotation {
  // KH 14:1
  final override val one        : Rotation = Rotation( 13, 10, 35)
  // KH 14:2
  final override val ten        : Rotation = Rotation(131, 45, 50)
  final override val hundred    : Rotation = Rotation(237, 38, 23)
  final override val thousand   : Rotation = Rotation(216, 23, 50)
  final override val tenThousand: Rotation = Rotation(  3, 58, 20)

  final override val month      : Rotation = Rotation( 22,  6, 56)
  final override val year       : Rotation = Rotation(344, 26, 43)

  // TODO does this correspond to the lunar period?

  final override val rambamValue = Rotation(13, 10, 35, 1, 48, 1)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 10, 35, 3) -
  // as explanation of the value for 100 days (3 extra seconds)

  final override val almagestValue = Rotation(13,10,34,58,33,30,30)
}
