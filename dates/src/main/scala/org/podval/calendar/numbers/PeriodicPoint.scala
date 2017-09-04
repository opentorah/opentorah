package org.podval.calendar.numbers

abstract class PeriodicPoint[S <: PeriodicNumberSystem[S]](digits: Seq[Int])
  extends PointBase[S](digits) with PeriodicNumber[S, S#Point]
{ this: S#Point =>
}
