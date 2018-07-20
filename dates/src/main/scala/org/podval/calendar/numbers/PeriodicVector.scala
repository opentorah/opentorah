package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumberSystem[S]]
  extends VectorBase[S] with PeriodicNumber[S, S#Vector]
{ this: S#Vector =>
}
