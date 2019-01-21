package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumbers[S]]
  extends VectorBase[S] with Number[S, S#Vector]
{ this: S#Vector =>
}
