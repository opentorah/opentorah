package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicVector

abstract class RotationBase(digits: Seq[Int])
  extends PeriodicVector[Angles](digits) with Angle[RotationBase]
