package org.podval.calendar.astronomy

import org.podval.calendar.angle.Angles.Rotation

object MoonAnomalyMean extends Days2Angle {
  // TODO opposite direction!

  // KH 14:3
  final override val one        : Rotation = Rotation( 13,  3, 54)
  final override val ten        : Rotation = Rotation(130, 39,  0)
  final override val hundred    : Rotation = Rotation(226, 29, 53)
  final override val thousand   : Rotation = Rotation(104, 58, 50)
  final override val tenThousand: Rotation = Rotation(329, 48, 20)

  final override val month      : Rotation = Rotation( 18, 53,  4)
  // KH 14:4
  final override val year       : Rotation = Rotation(305,  0, 13)

  final override def rounder(key: Days2Angle.Key): Rotation => Rotation = _.roundToSeconds

  final override val rambamValue: Rotation = Rotation(13, 3, 53, 55, 49)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 3, 53, 53) -
  // as explanation of the value for 100 days (7 missing seconds)

  final override val almagestValue = Rotation(13, 3, 53, 56, 17, 51, 59)
}
