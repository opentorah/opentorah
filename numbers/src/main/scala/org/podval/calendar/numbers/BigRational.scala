package org.podval.calendar.numbers

final class BigRational private(
  val negative: Boolean,
  val numerator: BigInt,
  val denominator: BigInt)
{
  require(numerator  .signum >= 0)
  require(denominator.signum >= 0)

  override def toString: String =
    (if (negative) "-" else "") + numerator.toString + "/" + denominator.toString

  override def equals(other: Any): Boolean = other match {
    case that: BigRational =>
      (this.negative     == that.negative) &&
      (this.numerator    == that.numerator) &&
      (this.denominator  == that.denominator)

    case _ => false
  }

  // TODO hashCode, compare...
}


object BigRational {
  final def apply(negative: Boolean, numerator: BigInt, denominator: BigInt): BigRational =
    new BigRational(negative, numerator, denominator)

  final def apply(numerator: BigInt, denominator: BigInt): BigRational =
    apply(false, numerator, denominator)
}
