package org.podval.calendar.dates

import org.podval.calendar.time.TimePointBase

abstract class MomentBase[C <: Calendar[C]](digits: Seq[Int])
  extends TimePointBase[C](digits) with CalendarMember[C]
{ this: C#Moment =>
  final def day: C#Day = calendar.Day(days + 1)

  final def time: C#TimeInterval = days(0).toInterval
}
