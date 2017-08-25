package org.podval.calendar.numbers

final class BigRational private(
  val negative: Boolean,
  val numerator: BigInt,
  val denominator: BigInt)
{
  require(numerator  .signum >= 0)
  require(denominator.signum >= 0)

  // TODO add abs() and use it in fromRational()

  // TODO add unary_-

  def setNegative(value: Boolean): BigRational =
    if (negative == value) this
    else BigRational(value, numerator, denominator)

  def wholeAndFraction: (Int, BigRational) = {
    val whole: Int = (numerator / denominator).bigInteger.intValueExact
    val fraction: BigRational = BigRational(negative, numerator - whole*denominator, denominator)
    (whole, fraction)
  }

  def *(multiplier: Int): BigRational = BigRational(negative, numerator*multiplier, denominator)

  def *(that: BigRational): BigRational = BigRational(
    this.negative != that.negative,
    this.numerator*that.numerator,
    this.denominator*that.denominator)

  def isZero: Boolean = numerator == 0

  // TODO take negativity into account; implement Comparable...
  def +(that: BigRational): BigRational = BigRational(
    negative = false,
    this.numerator*that.denominator+this.denominator*that.numerator,
    this.denominator*that.denominator
  )

  override def toString: String =
    (if (negative) "-" else "") + numerator.toString + "/" + denominator.toString

  override def equals(other: Any): Boolean = other match {
    case that: BigRational =>
      (this.negative     == that.negative) &&
      (this.numerator    == that.numerator) &&
      (this.denominator  == that.denominator)

    case _ => false
  }

  // TODO hashCode
}


object BigRational {
  final def apply(negative: Boolean, numerator: BigInt, denominator: BigInt): BigRational = {
    val gcd: BigInt = numerator.gcd(denominator)
    new BigRational(negative, numerator / gcd, denominator / gcd)
  }

  final def apply(numerator: BigInt, denominator: BigInt): BigRational =
    apply(negative = false, numerator, denominator)

  def round(whole: Int, fraction: BigRational): Int = {
    val isNotLessThanHalf: Boolean = (fraction.numerator / fraction.denominator).floatValue >= 0.5f
    whole + (if (isNotLessThanHalf) 1 else 0)
  }
}
