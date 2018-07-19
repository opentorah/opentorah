package org.podval.calendar.numbers

/**
  * Operations needed to convert to/from T.
  *
  * @tparam T  type of the number to convert to/from
  */
trait Convertible[T] {
  def zero: T
  def signum(value: T): Int
  def abs(value: T): T
  def plus(value: T, that: T): T
  def mult(value: T, n: Int): T
  def whole(value: T): Int
  def fraction(value: T): T
  def round(value: T): Int

  def div(digit: Int, denominator: BigInt): T
}

object Convertible {
  def apply[T](implicit ev: Convertible[T]): Convertible[T] = ev

  implicit class ConvertibleOps[T: Convertible](value: T) {
    def signum: Int = Convertible[T].signum(value)
    def abs: T = Convertible[T].abs(value)
    def +(that: T): T = Convertible[T].plus(value, that)
    def *(n: Int): T = Convertible[T].mult(value, n)
    def whole: Int = Convertible[T].whole(value)
    def fraction: T = Convertible[T].fraction(value)
    def round: Int = Convertible[T].round(value)
  }

  implicit val bigRationalConvertible: Convertible[BigRational] = new Convertible[BigRational] {
    override def zero: BigRational = BigRational.zero
    override def signum(value: BigRational): Int = value.signum
    override def abs(value: BigRational): BigRational = value.abs
    override def plus(value: BigRational, that: BigRational): BigRational = value + that
    override def mult(value: BigRational, n: Int): BigRational = value * BigRational(n)
    override def whole(value: BigRational): Int = value.whole
    override def fraction(value: BigRational): BigRational = value.fraction
    override def round(value: BigRational): Int = value.round

    override def div(digit: Int, denominator: BigInt): BigRational = BigRational(digit, denominator)
  }

  implicit val doubleConvertible: Convertible[Double] = new Convertible[Double] {
    override def zero: Double = 0
    override def signum(value: Double): Int = math.signum(value).toInt
    override def abs(value: Double): Double = math.abs(value)
    override def plus(value: Double, that: Double): Double = value + that
    override def mult(value: Double, n: Int): Double = value * n
    override def whole(value: Double): Int = math.floor(value).toInt
    override def fraction(value: Double): Double = value - whole(value)
    override def round(value: Double): Int = math.round(value).toInt

    override def div(digit: Int, denominator: BigInt): Double = digit.toDouble/denominator.bigInteger.longValueExact
  }
}
