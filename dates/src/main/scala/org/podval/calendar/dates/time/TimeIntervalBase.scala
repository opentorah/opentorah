package org.podval.calendar.dates.time

import org.podval.calendar.numbers.IntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class TimeIntervalBase[S <: TimeNumberSystem[S]](raw: RawNumber)
  extends IntervalBase[S](raw) with TimeNumber[S, S#Interval]
{
  this: S#Interval =>
}
