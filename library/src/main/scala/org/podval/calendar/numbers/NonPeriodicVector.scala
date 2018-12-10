package org.podval.calendar.numbers

trait NonPeriodicVector[S <: NonPeriodicNumbers[S]]
  extends VectorBase[S] with NonPeriodicNumber[S, S#Vector]
{ this: S#Vector =>
}
