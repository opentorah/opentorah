package org.podval.calendar.times

import org.podval.calendar.numbers.VectorBase

abstract class TimeVectorBase[S <: Times[S]](digits: Seq[Int])
  extends VectorBase[S](digits) with Time[S, S#Vector]
{
  this: S#Vector =>
}
