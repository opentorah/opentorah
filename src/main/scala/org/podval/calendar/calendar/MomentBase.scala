package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.TimePoint

abstract class MomentBase[C <: Calendar[C]](raw: RawNumber)
  extends TimePoint[C](raw) with CalendarMember[C]
{ this: C#Moment =>
  final def day: C#Day = calendar.createDay(days + 1)

  final def time: C#Interval = calendar.createInterval(false, days(0).digits)
}
