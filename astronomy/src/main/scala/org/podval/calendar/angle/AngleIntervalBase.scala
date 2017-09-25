package org.podval.calendar.angle

import org.podval.calendar.numbers.PeriodicInterval

abstract class AngleIntervalBase(digits: Seq[Int])
  extends PeriodicInterval[AngleNumberSystem](digits) with AngleNumber[AngleIntervalBase]
