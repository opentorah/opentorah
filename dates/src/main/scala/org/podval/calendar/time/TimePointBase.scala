package org.podval.calendar.time

import org.podval.calendar.numbers.PointBase

abstract class TimePointBase[S <: TimeNumberSystem[S]](negative: Boolean, digits: Seq[Int])
  extends PointBase[S](negative, digits) with TimeNumber[S, S#Point]
{
  this: S#Point =>
}
