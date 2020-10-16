package org.opentorah.calendar.gregorian

import org.opentorah.dates.MomentBase
import Gregorian.Moment

abstract class GregorianMoment(calendar: Gregorian, digits: Seq[Int])
  extends MomentBase[Gregorian](calendar, digits)
{
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)
}
