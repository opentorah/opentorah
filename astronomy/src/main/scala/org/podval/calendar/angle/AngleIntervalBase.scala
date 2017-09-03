package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicInterval

// TODO eliminate
// TODO rename AngleInterval
abstract class AngleIntervalBase(negative: Boolean, digits: Seq[Int])
  extends PeriodicInterval[AngleNumberSystem](negative, digits) with AngleNumber[AngleIntervalBase]
