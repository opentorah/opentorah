package org.podval.calendar.dates.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber

/**
  *
  */
abstract class MomentCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(raw: RawNumber): C#Moment = calendar.createMoment(raw)
}
