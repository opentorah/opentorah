package org.podval.calendar.numbers

trait RangedHeadDigitNumber[S <: RangedHeadDigitNumberSystem[S], N <: RangedHeadDigitNumber[S, N]]
  extends Number[S, N]
{ this: N =>
  final def complement: N = newNumber(
    !negative,
    (numberSystem.headRange - head) +:
      numberSystem.zipWithRanges(tail).map { case (digit, range) => range - digit }
  )

  final def canonical: N = if (!negative) this else this.complement

  final override def compare(that: N): Int = this.canonical.compareDigits(that.canonical)

  final override def hashCode: Int = canonical.digitsHashCode
}
