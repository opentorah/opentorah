package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicPoint

abstract class AnglePointBase(digits: Seq[Int])
  extends PeriodicPoint[AngleNumberSystem](digits) with AngleNumber[AnglePointBase]
