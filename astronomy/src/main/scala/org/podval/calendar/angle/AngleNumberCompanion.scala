package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberCompanion

trait AngleNumberCompanion[N <: AngleNumber[N]]
  extends NumberCompanion[AngleNumberSystem, N]
{
  final def fromRadians(value: Double, length: Int): N =
    fromDegrees(math.toDegrees(value), length)

  final def fromDegrees(value: Double, length: Int): N =
    newNumber(AngleNumberSystem.fromDouble(value, length))
}
