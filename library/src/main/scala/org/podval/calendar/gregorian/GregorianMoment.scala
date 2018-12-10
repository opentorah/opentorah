package org.podval.calendar.gregorian

import org.podval.calendar.dates.MomentBase
import Gregorian.Moment

trait GregorianMoment extends MomentBase[Gregorian] {
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)
}
