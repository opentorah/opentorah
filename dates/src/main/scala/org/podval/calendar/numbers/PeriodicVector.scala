package org.podval.calendar.numbers

abstract class PeriodicVector[S <: PeriodicNumberSystem[S]](digits: Seq[Int])
  extends VectorBase[S](digits) with PeriodicNumber[S, S#Vector]
{ this: S#Vector =>
}
