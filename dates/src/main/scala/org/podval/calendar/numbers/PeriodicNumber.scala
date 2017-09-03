package org.podval.calendar.numbers

trait PeriodicNumber[S <: PeriodicNumberSystem[S], N <: PeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>
  private[this] def headRange: Int = numberSystem.headRange

  final def normal: N = digit(0, head % headRange)

  final def canonical: N = if (!negative) this else this.complement

  final def symmetrical: N = if (head <= headRange/2) this else this.complement

  final def complement: N = newNumber(
    !negative,
    (headRange - head) +: zipWithRanges.map { case (digit, range) => range - digit }
  )

  final override def compare(that: N): Int =
    this.normal.canonical.compareDigits(that.normal.canonical)

  final override def hashCode: Int = this.normal.canonical.digitsHashCode
}
