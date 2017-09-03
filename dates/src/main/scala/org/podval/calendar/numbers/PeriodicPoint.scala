package org.podval.calendar.numbers

abstract class PeriodicPoint[S <: PeriodicNumberSystem[S]](negative: Boolean, digits: Seq[Int])
  extends PointBase[S](negative, digits) with PeriodicNumber[S, S#Point]
{ this: S#Point =>
}
