package org.podval.calendar.dates

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class TimeIntervalCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(raw: RawNumber): C#TimeInterval = calendar.createInterval(raw)

  final def apply(): C#TimeInterval = apply(false, List(0))
}
