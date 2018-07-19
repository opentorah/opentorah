package org.podval.calendar.time

import org.podval.calendar.numbers.PointBase

// TODO rename Moment
abstract class TimePointBase[S <: TimeNumberSystem[S]](digits: Seq[Int])
  extends PointBase[S](digits) with TimeNumber[S, S#Point]
{
  this: S#Point =>
}
