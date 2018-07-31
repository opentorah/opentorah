package org.podval.calendar.times

import org.podval.calendar.numbers.NonPeriodicVector

trait TimeVectorBase[S <: Times[S]]
  extends NonPeriodicVector[S] with Time[S, S#Vector]
{ this: S#Vector =>
}
