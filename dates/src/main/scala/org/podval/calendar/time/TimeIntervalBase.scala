package org.podval.calendar.time

import org.podval.calendar.numbers.IntervalBase

abstract class TimeIntervalBase[S <: TimeNumberSystem[S]](negative: Boolean, digits: Seq[Int])
  extends IntervalBase[S](negative, digits) with TimeNumber[S, S#Interval]
{
  this: S#Interval =>
}
