package org.podval.calendar.astronomy.angle

import AngleNumberSystem.Angle

class AngleCompanion {
  final def apply(digits: Int*): Angle = apply(negative = false, digits.toList)

  final def apply(negative: Boolean, digits: List[Int]): Angle =
    AngleNumberSystem.createInterval(negative, digits)

  final def fromRadians(value: Double, length: Int): Angle =
    fromDegrees(math.toDegrees(value), length)

  final def fromDegrees(value: Double, length: Int): Angle =
    AngleNumberSystem.newInterval(AngleNumberSystem.fromDouble(value, length))

  final def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullDays = 360.0/approximate.toDegrees
    val fullRotations = math.floor(days/fullDays).toInt
    (360.0*fullRotations+angle.toDegrees)/days
  }
}
