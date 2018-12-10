package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumbers[S]]
  extends VectorBase[S] with PeriodicNumber[S, S#Vector]
{ this: S#Vector =>
}
