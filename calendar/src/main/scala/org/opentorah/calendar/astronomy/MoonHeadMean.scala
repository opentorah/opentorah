package org.opentorah.calendar.astronomy

import org.opentorah.angles.Angles.Rotation

object MoonHeadMean extends Time2Rotation {

  // KH 16:2
  final override val one        : Rotation = Rotation(  0,  3, 11) // few thirds less (f6)
  final override val ten        : Rotation = Rotation(  0, 31, 47)
  final override val hundred    : Rotation = Rotation(  5, 17, 43)
  final override val thousand   : Rotation = Rotation( 52, 57, 10)
  final override val tenThousand: Rotation = Rotation(169, 31, 40)

  final override val month      : Rotation = Rotation(  1, 32,  9)
  final override val year       : Rotation = Rotation( 18, 44, 42)

  final override val rambamValue = Rotation(0)

  final override val almagestValue = Rotation(0)
}
