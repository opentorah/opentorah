package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.Rotation

object SunApogee extends Days2Angle {
  // KH 12:2
  final override val one        : Rotation = Rotation.zero // Rambam doesn't give this value
  final override val ten        : Rotation = Rotation(0,  0,  1, 30)
  final override val hundred    : Rotation = Rotation(0,  0, 15)
  final override val thousand   : Rotation = Rotation(0,  2, 30)
  final override val tenThousand: Rotation = Rotation(0, 25)

  final override val month      : Rotation = Rotation(0,  0,  4) // TODO: veod!
  final override val year       : Rotation = Rotation(0,  0, 53)

  final override def rounder(key: Days2Angle.Key): Rotation => Rotation =
    if (key == Days2Angle.Key.One) _.roundToThirds else _.roundToSeconds


  final override val rambamValue = Rotation(0) // TODO

  final override val almagestValue = Rotation(0) // TODO
}
