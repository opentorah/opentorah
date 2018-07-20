package org.podval.calendar.times

import org.podval.calendar.numbers.PointBase

abstract class TimePointBase[S <: Times[S]](digits: Seq[Int])
  extends PointBase[S](digits) with Time[S, S#Point]
{
  this: S#Point =>
}
