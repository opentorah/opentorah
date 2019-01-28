package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Digit, Rotation}
import Time2Rotation.Key
import org.podval.calendar.numbers.Digit

object SunApogee extends Time2Rotation {
  // KH 12:2
  final override val one        : Rotation = Rotation.zero // Rambam doesn't give this value
  final override val ten        : Rotation = Rotation(0,  0,  1, 30)
  final override val hundred    : Rotation = Rotation(0,  0, 15)
  final override val thousand   : Rotation = Rotation(0,  2, 30)
  final override val tenThousand: Rotation = Rotation(0, 25)

  final override val month      : Rotation = Rotation(0,  0,  4) // TODO: veod!
  final override val year       : Rotation = Rotation(0,  0, 53)

  protected override def precision(key: Key): Digit =
    if (key == Key.One) Digit.THIRDS else Digit.SECONDS

  final override val rambamValue = Rotation(0) // TODO

  final override val almagestValue = Rotation(0) // TODO
}
