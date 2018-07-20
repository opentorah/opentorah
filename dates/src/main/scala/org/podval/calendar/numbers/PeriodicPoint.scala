package org.podval.calendar.numbers

trait PeriodicPoint[S <: PeriodicNumberSystem[S]]
  extends PointBase[S] with PeriodicNumber[S, S#Point]
{ this: S#Point =>
}
