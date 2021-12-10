package org.opentorah.numbers

/**
  * Operations needed to convert to/from T.
  *
  * @tparam T  type of the number to convert to/from
  */
trait Convertible[T]:
  def signum(value: T): Int
  def abs(value: T): T
  def plus(value: T, that: T): T
  def mult(value: T, n: Int): T
  def div(digit: Int, denominator: BigInt): T
  def round(value: T): Int
  def wholeAndFraction(value: T): (Int, T)

object Convertible:
  given Convertible[BigRational] with
    override def signum(value: BigRational): Int = value.signum
    override def abs(value: BigRational): BigRational = value.abs
    override def plus(value: BigRational, that: BigRational): BigRational = value + that
    override def mult(value: BigRational, n: Int): BigRational = value * BigRational(n)
    override def div(digit: Int, denominator: BigInt): BigRational = BigRational(digit, denominator)
    override def round(value: BigRational): Int = value.round
    override def wholeAndFraction(value: BigRational): (Int, BigRational) = (value.whole, value.fraction)

  given Convertible[Double] with
    override def signum(value: Double): Int = math.signum(value).toInt
    override def abs(value: Double): Double = math.abs(value)
    override def plus(value: Double, that: Double): Double = value + that
    override def mult(value: Double, n: Int): Double = value * n
    override def div(digit: Int, denominator: BigInt): Double = digit.toDouble/denominator.bigInteger.longValueExact
    override def round(value: Double): Int = math.round(value).toInt
    override def wholeAndFraction(value: Double): (Int, Double) = 
      val whole = math.floor(value).toInt
      (whole, value - whole)
