package org.opentorah.calendar.gregorian

import org.opentorah.dates.MomentBase
import Gregorian.Moment

trait GregorianMoment extends MomentBase[Gregorian] {
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)
}
