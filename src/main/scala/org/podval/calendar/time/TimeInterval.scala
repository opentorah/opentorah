package org.podval.calendar.time

import org.podval.calendar.numbers.IntervalBase
import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class TimeInterval[S <: TimeNumberSystem[S]](raw: RawNumber)
  extends IntervalBase[S](raw) with TimeNumber[S, S#Interval]
{
  this: S#Interval =>
}
