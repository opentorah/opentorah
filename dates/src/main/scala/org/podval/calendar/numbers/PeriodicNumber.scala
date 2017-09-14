package org.podval.calendar.numbers

trait PeriodicNumber[S <: PeriodicNumberSystem[S], N <: PeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>
  final def complement(condition: Boolean): N = if (!condition) this else this.complement

  final def complement: N = fromDigits(numberSystem.complement(digits))

  final def symmetrical: N = complement(math.abs(head) > numberSystem.headRange/2)
}
