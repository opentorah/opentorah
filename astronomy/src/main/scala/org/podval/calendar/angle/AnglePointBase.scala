package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicPoint

// TODO rename AnglePoint - when there is no AngleNumberSystem.AnglePoint
abstract class AnglePointBase(negative: Boolean, digits: Seq[Int])
  extends PeriodicPoint[AngleNumberSystem](negative, digits) with AngleNumber[AnglePointBase]
