package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumbers[S]]
  extends VectorNumber[S] with Number[S, S#Vector]
{ this: S#Vector =>
}
