package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber

/**
  *
  */
abstract class MomentCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(raw: RawNumber): C#Moment = calendar.createMoment(raw)
}
