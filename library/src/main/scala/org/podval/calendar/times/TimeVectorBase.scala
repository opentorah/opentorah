package org.podval.calendar.times

import org.podval.calendar.numbers.VectorBase

trait TimeVectorBase[S <: Times[S]]
  extends VectorBase[S] with Time[S, S#Vector]
{ this: S#Vector =>
}
