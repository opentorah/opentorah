package org.podval.calendar.jewish

import org.podval.calendar.dates.MomentBase
import Jewish.Moment

abstract class JewishMoment(digits: Seq[Int]) extends MomentBase[Jewish](digits) {
  final def nightHours(value: Int): Moment = firstHalfHours(value)

  final def dayHours(value: Int): Moment = secondHalfHours(value)
}
