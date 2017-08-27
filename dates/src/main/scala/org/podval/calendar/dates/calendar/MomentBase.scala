package org.podval.calendar.dates.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.TimePointBase

abstract class MomentBase[C <: Calendar[C]](raw: RawNumber)
  extends TimePointBase[C](raw) with CalendarMember[C]
{ this: C#Moment =>
  final def day: C#Day = calendar.createDay(days + 1)

  final def time: C#TimeInterval = calendar.newInterval(false, days(0).digits)
}
