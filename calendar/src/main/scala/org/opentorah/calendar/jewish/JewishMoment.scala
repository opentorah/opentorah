package org.opentorah.calendar.jewish

import org.opentorah.calendar.dates.MomentBase
import Jewish.Moment

trait JewishMoment extends MomentBase[Jewish] {
  final def nightHours(value: Int): Moment = firstHalfHours(value)

  final def dayHours(value: Int): Moment = secondHalfHours(value)
}
