package org.podval.calendar.jewish

import org.podval.calendar.calendar.MomentBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class JewishMoment(raw: RawNumber) extends MomentBase[Jewish](raw) {
  final def nightHours(value: Int): Jewish.Moment = firstHalfHours(value)

  final def dayHours(value: Int): Jewish.Moment = secondHalfHours(value)
}
