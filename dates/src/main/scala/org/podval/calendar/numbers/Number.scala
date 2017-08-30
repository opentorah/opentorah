package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]](raw: RawNumber) extends Ordered[N]
{ this: N =>
  def numberSystem: S

  protected final def newPoint(raw: RawNumber): S#Point = numberSystem.newPoint(raw)
  protected final def newInterval(raw: RawNumber): S#Interval = numberSystem.newInterval(raw)

  protected def newNumber(raw: RawNumber): N

  final def negative: Boolean = raw._1

  final def signum: Int = if ((length == 0) && (head == 0)) 0 else NumberSystem.signum(negative)

  final def digits: List[Int] = raw._2

  final def head: Int = digits.head

  final def tail: List[Int] = digits.tail

  final def length: Int = tail.length

  final def digit(position: Int): Int = if (length >= position) digits(position) else 0

  final def digit(position: Int, value: Int): N =
    newNumber(negative, digits.padTo(position + 1, 0).updated(position, value))

  final def unary_- : N = newNumber(!negative, digits)

  protected final def add(negate: Boolean, that: Number[S, _]): RawNumber = {
    val sameSign: Boolean = this.negative == that.negative
    val operationSelector: Boolean = if (negate) !sameSign else sameSign
    val operation: (Int, Int) => Int = if (operationSelector) _ + _ else _ - _
    (negative, zip(that).map(operation.tupled))
  }

  // TODO add rounding tests
  final def roundTo(length: Int): N = {
    require(length >= 0)

    val (more_, toRound) = tail splitAt length
    val tail_ = {
      if (more_.isEmpty) more_ else {
        val toRoundWithRange = toRound.zipWithIndex.map {
          case (digit, position) => (digit, numberSystem.range(length+position))
        }
        val carry =
          (toRoundWithRange :\ 0) { case ((x, range), c) => if (x + c >= range / 2) 1 else 0}
        more_.init :+ (more_.last + carry)
      }
    }

    newNumber(negative, head +: tail_)
  }

  final def toRational: BigRational = {
    def forDigit(digit: Int, denominator: BigInt): BigRational =
      BigRational(digit, denominator)
    to[BigRational](forDigit, _ + _) * signum
  }

  final def toDouble: Double = {
    def forDigit(digit: Int, denominator: BigInt): Double =
      digit.toDouble/denominator.bigInteger.longValueExact()
    to[Double](forDigit, _ + _) * signum
  }

  private[this] final def to[T](forDigit /* rename digitTo */: (Int, BigInt) => T, plus: (T, T) => T): T = {
    val zeroDenominator: BigInt = BigInt(1)
    numberSystem.zipWithRanges(tail).foldLeft((forDigit(head, zeroDenominator), zeroDenominator)) {
      case ((acc: T, denominator: BigInt), (digit: Int, range: Int)) =>
        val newDenominator: BigInt = denominator*range
        (plus(acc, forDigit(digit, newDenominator)), newDenominator)
    }._1
  }

  private[this] def zip(that: Number[S, _]): List[(Int, Int)] =
    this.digits zipAll(that.digits, 0, 0)

  // TODO why can't I inline .tupled?
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled

  // TODO: padding with 0 to a given length
  protected final def toSignedString: String = {
    val digitsWithSigns: List[(Int, Option[String])] = tail.zipWithIndex.map {
      case (digit, position) => (digit, numberSystem.sign(position))
    }
    val tailResult: List[String] = if (digitsWithSigns.isEmpty) List.empty else
      digitsWithSigns.init.map { case (digit, sign) => digit + sign.getOrElse(",")} :+
        { val (digit, sign) = digitsWithSigns.last; digit + sign.getOrElse("") }

    val result: List[String] = (head + numberSystem.headSign) +: tailResult

    (if (negative) "-" else "") + result.mkString
  }

  override def toString: String = toSignedString

  // This needs to be overridden for the RangedHeadDigitNumber, so it isn't final.
  override def hashCode: Int = digitsHashCode

  final def digitsHashCode: Int = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode

  // This needs to be overridden for the RangedHeadDigitNumber, so it isn't final.
  def compare(that: N): Int =
    (if (this.negative != that.negative) 1 else compareDigits(that)) * signum

  final def compareDigits(that: N): Int =
    zip(that).map(lift(_ compare _)).find (_ != 0) getOrElse 0

  final override def equals(other: Any): Boolean =
    // TODO deal with the "erasure" warning; compare numberSystem...
    if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0
}
