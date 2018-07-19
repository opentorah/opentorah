package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberCompanion

trait AngleCompanion[N <: Angle[N]] extends NumberCompanion[AngleNumberSystem, N] {
  final def fromRadians(value: Double, length: Int): N = fromDegrees(math.toDegrees(value), length)

  final def fromDegrees(value: Double, length: Int): N = fromDouble(value, length)
}
