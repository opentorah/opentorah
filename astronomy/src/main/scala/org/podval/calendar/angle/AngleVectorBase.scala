package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicVector

abstract class AngleVectorBase(digits: Seq[Int])
  extends PeriodicVector[AngleNumberSystem](digits) with AngleNumber[AngleVectorBase]
