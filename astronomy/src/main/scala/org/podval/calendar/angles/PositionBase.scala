package org.podval.calendar.angles

import org.podval.calendar.numbers.PeriodicPoint

abstract class PositionBase(digits: Seq[Int])
  extends PeriodicPoint[Angles](digits) with Angle[PositionBase]
