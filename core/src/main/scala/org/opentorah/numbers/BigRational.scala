package org.opentorah.numbers

/**
  * Arbitrary precision Rational number.
  *
  * Numerator is a BigInteger; denominator is a non-zero BigInteger.
  * If numerator is zero, resulting BigRational is zero (denominator is ignored).
  *
  * Representation is canonical:
  * - sign of the BigRational is kept in the numerator;
  * - denominator is always positive;
  * - numerator and denominator do not have common divisors.
  *
  * @param numerator  of the number (signed)
  * @param denominator  of the number (positive)
  */
final class BigRational private(val numerator: BigInt, val denominator: BigInt)
  extends Ordered[BigRational] derives CanEqual:
  // Representation invariants
  require(denominator > 0)
  require(numerator.gcd(denominator) == 1)

  override def toString: String = s"$numerator/$denominator"

  override def compare(that: BigRational): Int = (this - that).signum

  override def equals(other: Any): Boolean = compare(other.asInstanceOf[BigRational]) == 0

  def signum: Int = numerator.signum

  def isZero: Boolean = signum == 0

  def isPositive: Boolean = signum > 0

  def isNegative: Boolean = signum < 0

  def abs: BigRational = BigRational(numerator.abs, denominator)

  @scala.annotation.targetName("uminus")
  def unary_- : BigRational = BigRational(-numerator, denominator)

  @scala.annotation.targetName("add")
  def +(that: BigRational): BigRational = BigRational(
    this.numerator * that.denominator + that.numerator * this.denominator,
    this.denominator * that.denominator
  )

  @scala.annotation.targetName("subtract")
  def -(that: BigRational): BigRational = this + -that

  def invert: BigRational = BigRational(denominator, numerator)

  @scala.annotation.targetName("multiply")
  def *(that: BigRational): BigRational = BigRational(
    numerator = this.numerator * that.numerator,
    denominator = this.denominator * that.denominator
  )

  @scala.annotation.targetName("divide")
  def /(that: BigRational): BigRational = this * that.invert

  def whole: Int = (numerator / denominator).bigInteger.intValueExact

  def fraction: BigRational = this - BigRational(whole)

  def round: Int = whole + (if fraction.abs <= BigRational.oneHalf then 0 else fraction.signum)

  def toDouble: Double = numerator.toDouble / denominator.toDouble


object BigRational:
  // Used in apply(), so it is instantiated directly to avoid cycle (and thus `null` value) during initialization.
  val zero: BigRational = new BigRational(0, 1)

  val oneHalf: BigRational = BigRational(1, 2)

  val one: BigRational = BigRational(1, 1)

  final def apply(numerator: BigInt, denominator: BigInt): BigRational =
    if denominator == 0 then throw ArithmeticException("Division by 0")
    if numerator == 0 then zero else
      val gcd: BigInt = numerator.gcd(denominator)
      new BigRational(
        numerator = (numerator / gcd) * denominator.signum,
        denominator = (denominator / gcd).abs
      )

  final def apply(numerator: Int): BigRational = apply(numerator, 1)

  final def apply(string: String): BigRational = fromString(string)

  final def fromString(string: String): BigRational =
    val stringTrimmed: String = string.trim
    val isNegative: Boolean = stringTrimmed.startsWith("-")
    val values: Array[String] = (if isNegative then stringTrimmed.substring(1) else stringTrimmed).split('/')
    val numeratorAbs: BigInt = BigInt(values(0).trim)
    val numerator: BigInt = if isNegative then -numeratorAbs else numeratorAbs
    if values.length == 2
    then apply(numerator, BigInt(values(1).trim))
    else apply(numerator, BigInt(1)             )

  final def continuedFraction(value: BigRational, length: Int): Digits =
    require(length >= 1)
    val whole: Int = value.whole
    val fraction: BigRational = value.fraction
    if fraction.isZero || (length == 1) then Seq(whole)
    else whole +: continuedFraction(fraction.invert, length-1)
