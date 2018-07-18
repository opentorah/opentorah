package org.podval.calendar.numbers

/**
  * Arbitrary precision Rational number.
  *
  * Numerator is a BigInteger; denominator is a non-zero BigInteger.
  * If numerator is zero, resulting BigRational is zero (denominator is ignored).
  *
  * Representation is canonical:
  * - sign of the BigRational is kept in the numerator; denominator is always positive;
  * - numerator and denominator do not have common divisors.
  *
  * @param numerator  of the number (signed)
  * @param denominator  of the number (positive)
  */
final case class BigRational private(numerator: BigInt, denominator: BigInt)
  extends Comparable[BigRational] with Ordered[BigRational]
{
  def signum: Int = numerator.signum

  def abs: BigRational = BigRational(numerator.abs, denominator)

  def unary_- : BigRational = BigRational(-numerator, denominator)

  def +(that: BigRational): BigRational = BigRational(
    this.numerator * that.denominator + that.numerator * this.denominator,
    this.denominator * that.denominator
  )

  def -(that: BigRational): BigRational = this + -that

  def invert: BigRational = BigRational(denominator, numerator)

  def *(that: BigRational): BigRational = BigRational(
    numerator = this.numerator * that.numerator,
    denominator = this.denominator * that.denominator
  )

  def /(that: BigRational): BigRational = this * that.invert

  def *(that: Int): BigRational = this * BigRational(that)

  def /(that: Int): BigRational = this / BigRational(that)

  def whole: Int = (numerator / denominator).bigInteger.intValueExact

  def fraction: BigRational = this - BigRational(whole)

  def round: Int = if (fraction.abs <= BigRational.oneHalf) whole else whole + fraction.signum

  override def toString: String = numerator + "/" + denominator

  override def compare(that: BigRational): Int = (this - that).signum

  override def equals(other: Any): Boolean = other match {
    case that: BigRational => compare(that) == 0
    case _ => false
  }

  override def hashCode: Int = 73*numerator.hashCode + 31*denominator.hashCode
}


object BigRational {
  // This is instantiated directly to avoid cycle (and thus `null` value) during initialization.
  val zero: BigRational = new BigRational(0, 1)

  val oneHalf: BigRational = BigRational(1, 2)

  val one: BigRational = BigRational(1, 1)

  final def apply(numerator: BigInt, denominator: BigInt): BigRational = {
    if (denominator == 0) throw new ArithmeticException("Division by 0")
    if (numerator == 0) zero else {
      val gcd: BigInt = numerator.gcd(denominator)
      new BigRational(
        numerator = (numerator / gcd) * denominator.signum,
        denominator = (denominator / gcd).abs
      )
    }
  }

  final def apply(numerator: Int): BigRational = apply(numerator, 1)

  final def apply(value: String): BigRational = {
    val values = value.split('/')
    if (values.length != 2) throw new ArithmeticException(s"Invalid BigRational: $value")
    apply(BigInt(values(0).trim), BigInt(values(1).trim))
  }
}
