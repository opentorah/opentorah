package org.podval.calendar.dates

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.TimePointBase

abstract class MomentBase[C <: Calendar[C]](raw: RawNumber)
  extends TimePointBase[C](raw) with CalendarMember[C]
{ this: C#Moment =>
  final def day: C#Day = calendar.createDay(days + 1)

  // TODO why not just days(0)?
  final def time: C#TimeInterval = calendar.TimeInterval.newNumber(false, days(0).digits)
}
