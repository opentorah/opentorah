package org.podval.calendar.numbers

/**
  * Operations needed to convert to/from T.
  *
  * @tparam T  type of the number to convert to/from
  */
trait Convertible[T] {
  def signum(value: T): Int
  def abs(value: T): T
  def plus(value: T, that: T): T
  def mult(value: T, n: Int): T
  def forDigit(digit: Int, denominator: BigInt): T
  // TODO simplify: define in the implicits; round the value itself; split wholeAndFraction...
  def wholeAndFraction(value: T): (Int, T)
  def round(whole: Int, fraction: T): Int
}

object Convertible {
  // TODO add ConvertibleOps class - or not to bother?

  implicit val bigRationalConvertible: Convertible[BigRational] = new Convertible[BigRational] {
    override def signum(value: BigRational): Int = value.signum
    override def abs(value: BigRational): BigRational = value.abs
    override def plus(value: BigRational, that: BigRational): BigRational = value + that
    override def mult(value: BigRational, n: Int): BigRational = value * n
    override def wholeAndFraction(value: BigRational): (Int, BigRational) = value.wholeAndFraction
    override def round(whole: Int, fraction: BigRational): Int = BigRational.round(whole, fraction)
    override def forDigit(digit: Int, denominator: BigInt): BigRational = BigRational(digit, denominator)
  }

  implicit val doubleConvertible: Convertible[Double] = new Convertible[Double] {
    override def signum(value: Double): Int = math.signum(value).toInt
    override def abs(value: Double): Double = math.abs(value)
    override def plus(value: Double, that: Double): Double = value + that
    override def mult(value: Double, n: Int): Double = value * n
    override def wholeAndFraction(value: Double): (Int, Double) = {
      val whole: Double = math.floor(value)
      (whole.toInt, value - whole)
    }
    override def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt
    override def forDigit(digit: Int, denominator: BigInt): Double = digit.toDouble/denominator.bigInteger.longValueExact
  }
}
