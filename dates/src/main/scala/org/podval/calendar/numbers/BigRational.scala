package org.podval.calendar.numbers

/**
  * TODO
  * @param numerator
  * @param denominator
  */
final case class BigRational private(numerator: BigInt, denominator: BigInt)
  extends Comparable[BigRational] with Ordered[BigRational]
{
  def signum: Int = numerator.signum

  def abs: BigRational = BigRational(numerator.abs, denominator)

  def unary_- : BigRational = BigRational(-numerator, denominator)

  def +(that: BigRational): BigRational = BigRational(
    this.numerator*that.denominator+that.numerator*this.denominator,
    this.denominator*that.denominator
  )

  def -(that: BigRational): BigRational = this + -that

  def invert: BigRational = BigRational(denominator, numerator)

  def *(that: BigRational): BigRational = BigRational(
    this.numerator*that.numerator,
    this.denominator*that.denominator
  )

  def *(that: Int): BigRational = this * BigRational(that)

  def /(that: BigRational): BigRational = this * that.invert

  def /(that: Int): BigRational = this / BigRational(that)

  def wholeAndFraction: (Int, BigRational) = {
    val whole: Int = (numerator / denominator).bigInteger.intValueExact
    val fraction: BigRational = BigRational(numerator - whole*denominator, denominator)
    (whole, fraction)
  }

  override def toString: String = numerator.toShort + "/" + denominator.toString

  override def compare(that: BigRational): Int =
    (this.numerator*that.denominator).compareTo(that.numerator*this.denominator)

  override def equals(other: Any): Boolean = other match {
    case that: BigRational =>
      (this.numerator    == that.numerator) && (this.denominator  == that.denominator)

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
        (numerator / gcd) * denominator.signum,
        (denominator / gcd).abs
      )
    }
  }

  final def apply(numerator: Int): BigRational = apply(numerator, 1)

  final def apply(numerator: Long): BigRational = apply(numerator, 1)

  /**
    * Accepts return of a call to wholeAndFraction().
    * @param whole
    * @param fraction
    * @return
    */
  def round(whole: Int, fraction: BigRational): Int = whole + (if (fraction >= oneHalf) 1 else 0)
}
