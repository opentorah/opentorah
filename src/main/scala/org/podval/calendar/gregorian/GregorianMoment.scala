package org.podval.calendar.gregorian

import org.podval.calendar.calendar.MomentBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class GregorianMoment(raw: RawNumber)
  extends MomentBase[Gregorian](raw)
{ this: Gregorian#Moment =>
  final def morningHours(value: Int): Gregorian#Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Gregorian#Moment = secondHalfHours(value)
}
