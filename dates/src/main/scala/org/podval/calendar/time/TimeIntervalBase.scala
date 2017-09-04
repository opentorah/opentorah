package org.podval.calendar.time

import org.podval.calendar.numbers.IntervalBase

abstract class TimeIntervalBase[S <: TimeNumberSystem[S]](digits: Seq[Int])
  extends IntervalBase[S](digits) with TimeNumber[S, S#Interval]
{
  this: S#Interval =>
}
