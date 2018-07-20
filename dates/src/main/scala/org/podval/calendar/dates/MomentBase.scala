package org.podval.calendar.dates

import org.podval.calendar.times.TimePointBase

trait MomentBase[C <: Calendar[C]] extends TimePointBase[C] with CalendarMember[C]
{ this: C#Moment =>
  final def day: C#Day = calendar.Day(days + 1)
}
