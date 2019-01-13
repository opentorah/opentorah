package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumbers[S]]
  extends VectorBase[S] with PeriodicNumber[S, S#Vector]
{ this: S#Vector =>
  final def canonical: S#Vector = fromDigits(numbers.normalize(digits, isCanonical = true))
}
