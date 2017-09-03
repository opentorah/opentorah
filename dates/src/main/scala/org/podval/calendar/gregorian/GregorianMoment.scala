package org.podval.calendar.gregorian

import org.podval.calendar.dates.MomentBase
import Gregorian.Moment

abstract class GregorianMoment(negative: Boolean, digits: Seq[Int])
  extends MomentBase[Gregorian](negative, digits)
{
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)
}
