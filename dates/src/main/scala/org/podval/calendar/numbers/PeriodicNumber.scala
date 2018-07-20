package org.podval.calendar.numbers

trait PeriodicNumber[S <: PeriodicNumbers[S], N <: PeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>
  final def symmetrical: N = fromDigits(numbers.symmetrical(this))
}
