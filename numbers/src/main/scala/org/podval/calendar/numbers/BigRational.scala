package org.podval.calendar.numbers

final class BigRational private(
  val negative: Boolean,
  val numerator: BigInt,
  val denominator: BigInt)
{
  require(numerator  .signum >= 0)
  require(denominator.signum >= 0)

  def wholeAndFraction: (Int, BigRational) = {
    val whole: Int = (numerator / denominator).bigInteger.intValueExact
    val fraction: BigRational = BigRational(negative, numerator - whole*denominator, denominator)
    (whole, fraction)
  }

  def *(multiplier: Int): BigRational = BigRational(negative, numerator*multiplier, denominator)

  def isZero: Boolean = numerator == 0

  def isNotLessThanHalf: Boolean = (numerator / denominator).floatValue >= 0.5f

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
  final def apply(negative: Boolean, numerator: BigInt, denominator: BigInt): BigRational = {
    val gcd: BigInt = numerator.gcd(denominator)
    new BigRational(negative, numerator / gcd, denominator / gcd)
  }

  final def apply(numerator: BigInt, denominator: BigInt): BigRational =
    apply(negative = false, numerator, denominator)
}
