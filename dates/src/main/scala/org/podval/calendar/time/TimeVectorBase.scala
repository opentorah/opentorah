package org.podval.calendar.time

import org.podval.calendar.numbers.VectorBase

abstract class TimeVectorBase[S <: TimeNumberSystem[S]](digits: Seq[Int])
  extends VectorBase[S](digits) with TimeNumber[S, S#Vector]
{
  this: S#Vector =>
}
