package org.podval.calendar.gregorian

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.dates.MomentBase
import Gregorian.Moment

abstract class GregorianMoment(raw: RawNumber)
  extends MomentBase[Gregorian](raw)
{
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)
}
