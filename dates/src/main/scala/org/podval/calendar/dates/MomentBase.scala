package org.podval.calendar.dates

import org.podval.calendar.times.TimePointBase

trait MomentBase[C <: Calendar[C]] extends TimePointBase[C]
{ this: C#Moment =>
  final def calendar: C = numbers
  final def day: C#Day = calendar.Day(days + 1)
}
