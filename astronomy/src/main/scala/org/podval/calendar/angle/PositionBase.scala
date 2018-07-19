package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicPoint

abstract class PositionBase(digits: Seq[Int])
  extends PeriodicPoint[AngleNumberSystem](digits) with Angle[PositionBase]
