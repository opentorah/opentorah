package org.podval.calendar.numbers

trait PeriodicNumber[S <: PeriodicNumberSystem[S], N <: PeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>
  private[this] def headRange: Int = numberSystem.headRange

  final def normal: N = head(head % headRange) // TODO verify with negatives...

  final def canonical: N = complement(isNegative)

  final def symmetrical: N = complement(absHead > headRange/2)

  final def complement(condition: Boolean): N = if (!condition) this else this.complement

  final def complement: N = newNumber(
    -signum*(headRange - absHead) +:
      zipWithRanges.map { case (digit, range) => range - digit }
  )

  final override def compare(that: N): Int =
    this.normal.canonical.compareDigits(that.normal.canonical)

  final override def hashCode: Int = this.normal.canonical.digitsHashCode
}
