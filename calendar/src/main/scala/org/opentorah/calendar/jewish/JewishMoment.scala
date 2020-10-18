package org.opentorah.calendar.jewish

import org.opentorah.dates.MomentBase
import Jewish.Moment

abstract class JewishMoment(digits: Seq[Int]) extends MomentBase[Jewish](digits) {
  final def nightHours(value: Int): Moment = firstHalfHours(value)

  final def dayHours(value: Int): Moment = secondHalfHours(value)
}
