package org.podval.calendar.numbers

final class BigRational private(
  val negative: Boolean,
  val numerator: BigInt,
  val denominator: BigInt) extends Comparable[BigRational] with Ordered[BigRational]
{
  def signum: Int = if (numerator == 0) 0 else NumberSystem.signum(negative)

  def abs: BigRational = setNegative(false)

  def unary_- : BigRational = setNegative(!negative)

  def setNegative(value: Boolean): BigRational =
    if (negative == value) this
    else BigRational(value, numerator, denominator)

  def +(that: BigRational): BigRational = {
    val numerator: BigInt =
      this.signum*this.numerator*that.denominator+that.signum*that.numerator*this.denominator
    BigRational(numerator < 0, numerator.abs, this.denominator*that.denominator)
  }

  def -(that: BigRational): BigRational = this + -that

  def invert: BigRational = BigRational(negative, denominator, numerator)

  def *(that: BigRational): BigRational = BigRational(
    this.negative != that.negative,
    this.numerator*that.numerator,
    this.denominator*that.denominator)

  def *(that: Int): BigRational = this * BigRational(that)

  def /(that: BigRational): BigRational = this * that.invert

  def /(that: Int): BigRational = this / BigRational(that)

  // TODO take negativity into account
  def wholeAndFraction: (Int, BigRational) = {
    val whole: Int = (numerator / denominator).bigInteger.intValueExact
    val fraction: BigRational = BigRational(negative, numerator - whole*denominator, denominator)
    (whole, fraction)
  }

  override def toString: String =
    (if (negative) "-" else "") + numerator.toString + "/" + denominator.toString

  override def compare(that: BigRational): Int = {
    (this.signum, that.signum) match {
      case ( 1,  1) => (this.numerator*that.denominator).compareTo(this.denominator*that.numerator)
      case (-1, -1) => -this.abs.compareTo(that.abs)
      case ( 0,  s) => -s
      case ( s,  _) =>  s
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that: BigRational =>
      (this.negative     == that.negative) &&
      (this.numerator    == that.numerator) &&
      (this.denominator  == that.denominator)

    case that: Int =>
      (denominator == 1) && (numerator == math.abs(that)) && (signum == math.signum(that))

    case that: Long =>
      (denominator == 1) && (numerator == math.abs(that)) && (signum == math.signum(that))

    case _ => false
  }

  override def hashCode: Int = 73*numerator.hashCode + 31*denominator.hashCode + negative.hashCode
}


object BigRational {
  // This is instantiated directly to avoid cycle (and thus `null` value) during initialization.
  val zero: BigRational = new BigRational(negative = false, 0, 1)

  val oneHalf: BigRational = BigRational(1, 2)

  final def apply(negative: Boolean, numerator: BigInt, denominator: BigInt): BigRational = {
    if (denominator < 0) throw new ArithmeticException(s"Negative denominator $denominator")
    if (denominator == 0) throw new ArithmeticException("Division by 0")
    if (numerator < 0) throw new ArithmeticException(s"Negative numerator $numerator")
    if (numerator == 0) zero else {
      val gcd: BigInt = numerator.gcd(denominator)
      new BigRational(negative, numerator / gcd, denominator / gcd)
    }
  }

  final def apply(numerator: BigInt, denominator: BigInt): BigRational =
    apply(negative = false, numerator, denominator)


  final def apply(numerator: Int): BigRational = apply(numerator < 0, math.abs(numerator), 1)

  final def apply(numerator: Long): BigRational = apply(numerator < 0, math.abs(numerator), 1)

  /**
    * Accepts return of a call to wholeAndFraction().
    * @param whole
    * @param fraction
    * @return
    */
  def round(whole: Int, fraction: BigRational): Int = whole + (if (fraction >= oneHalf) 1 else 0)
}
