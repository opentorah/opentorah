package org.opentorah.calendar.angles

import org.opentorah.numbers.NumberCompanion

trait AngleCompanion[N <: Angle[N]] extends NumberCompanion[Angles, N] {
  final def fromRadians(value: Double, length: Int): N = fromDegrees(math.toDegrees(value), length)

  final def fromDegrees(value: Double, length: Int): N = fromDouble(value, length)
}
