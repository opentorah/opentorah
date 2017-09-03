package org.podval.calendar.angle

import AngleNumberSystem.Angle
import org.podval.calendar.numbers.IntervalCompanion

abstract class AngleCompanion
  extends IntervalCompanion[AngleNumberSystem] with AngleNumberCompanion[Angle]
{
  // TODO generalize this into PeriodicInterval
  // TODO rework in my numbers?
  final def exactify(approximate: Angle, days: Int, angle: Angle): Double = {
    val fullRotations = math.floor(days*approximate.toDouble/numberSystem.headRange.toDouble).toInt
    (numberSystem.headRange.toDouble*fullRotations + angle.toDegrees)/days
  }
}
