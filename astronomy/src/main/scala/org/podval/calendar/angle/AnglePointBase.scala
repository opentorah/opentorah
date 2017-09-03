package org.podval.calendar.angle

import org.podval.calendar.numbers.RangedHeadDigitPoint

// TODO rename AnglePoint - when there is no AngleNumberSystem.AnglePoint
abstract class AnglePointBase(negative: Boolean, digits: Seq[Int])
  extends RangedHeadDigitPoint[AngleNumberSystem](negative, digits) with AngleNumber[AnglePointBase]
