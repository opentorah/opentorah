package org.podval.calendar.times

import org.podval.calendar.numbers.VectorNumber

trait TimeVectorBase[S <: Times[S]]
  extends VectorNumber[S] with Time[S, S#Vector]
{ this: S#Vector =>
}
