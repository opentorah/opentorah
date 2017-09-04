package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicPoint

// TODO eliminate
// TODO rename AnglePoint - when there is no AngleNumberSystem.AnglePoint
abstract class AnglePointBase(digits: Seq[Int])
  extends PeriodicPoint[AngleNumberSystem](digits) with AngleNumber[AnglePointBase]
