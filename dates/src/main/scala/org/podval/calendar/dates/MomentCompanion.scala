package org.podval.calendar.dates

import org.podval.calendar.numbers.NumberSystem.RawNumber

/**
  *
  */
abstract class MomentCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(raw: RawNumber): C#Moment = calendar.createMoment(raw)

  final def apply(): C#Moment = apply(false, List(0))
}
