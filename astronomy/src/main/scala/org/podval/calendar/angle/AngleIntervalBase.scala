package org.podval.calendar.angle

import org.podval.calendar.numbers.RangedHeadDigitInterval

// TODO rename AngleInterval
abstract class AngleIntervalBase(negative: Boolean, digits: Seq[Int])
  extends RangedHeadDigitInterval[AngleNumberSystem](negative, digits) with AngleNumber[AngleIntervalBase]
