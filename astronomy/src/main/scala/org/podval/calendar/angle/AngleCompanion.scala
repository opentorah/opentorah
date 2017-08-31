package org.podval.calendar.angle

import AngleNumberSystem.Angle
import org.podval.calendar.numbers.IntervalCompanionBase

abstract class AngleCompanion extends IntervalCompanionBase[AngleNumberSystem] {
  // TODO move the methods and eliminate?

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
