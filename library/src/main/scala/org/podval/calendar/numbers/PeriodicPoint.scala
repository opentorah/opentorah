package org.podval.calendar.numbers

trait PeriodicPoint[S <: PeriodicNumbers[S]]
  extends PointNumber[S] with Number[S, S#Point]
{ this: S#Point =>
  final def reflect: S#Point = fromDigits(numbers.period.subtract(this))
}
