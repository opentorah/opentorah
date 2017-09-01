package org.podval.calendar.angle

import AngleNumberSystem.Angle
import org.podval.calendar.numbers.IntervalCompanion

abstract class AngleCompanion
  extends IntervalCompanion[AngleNumberSystem] with AngleNumberCompanion[Angle]
{
  final def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullDays = 360.0/approximate.toDegrees
    val fullRotations = math.floor(days/fullDays).toInt
    (360.0*fullRotations+angle.toDegrees)/days
  }
}
