package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicInterval

// TODO eliminate
// TODO rename AngleInterval
abstract class AngleIntervalBase(digits: Seq[Int])
  extends PeriodicInterval[AngleNumberSystem](digits) with AngleNumber[AngleIntervalBase]
