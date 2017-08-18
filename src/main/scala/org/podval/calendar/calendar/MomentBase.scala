package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.TimePoint

abstract class MomentBase[T <: Calendar[T]](raw: RawNumber) extends TimePoint[T](raw) with CalendarMember[T]
{ this: T#Moment =>
  final def day: T#Day = calendar.createDay(days + 1)

  final def time: T#Interval = calendar.createInterval(false, days(0).digits)
}
