package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class Number[S <: NumberSystem[S], N <: Number[S, N]](raw: RawNumber) extends Ordered[N]
{ this: N =>
  def numberSystem: S

  protected final def newPoint(raw: RawNumber): S#Point = numberSystem.newPoint(raw)
  protected final def newInterval(raw: RawNumber): S#Interval = numberSystem.newInterval(raw)

  protected def newNumber(raw: RawNumber): N

  final def negative: Boolean = raw._1

  final def signum: Int = NumberSystem.signum(negative)

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

  final def toDouble: Double = signum * digits.zipWithIndex.map { case (digit, position) =>
    digit.toDouble / numberSystem.multiplier(position).bigInteger.longValueExact()
  }.sum

  private[this] def zip(that: Number[S, _]): List[(Int, Int)] =
    this.digits zipAll(that.digits, 0, 0)

  // TODO why can't I inline .tupled?
  private[this] def lift[A, B, C](op: (A, B) => C): (((A, B)) => C) = op.tupled

  final def toRational: BigRational = {
    val (numerator: BigInt, denominator: BigInt) =
      numberSystem.zipWithRanges(tail).foldLeft((BigInt(head), BigInt(1))) {
        case ((numerator: BigInt, denominator: BigInt), (digit: Int, range: Int)) =>
          (numerator*range + digit, denominator*range)
      }
    BigRational(negative, numerator, denominator)
  }

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

  final override def hashCode: Int = (73 /: digits)((v, x) => 41 * v + x) + negative.hashCode

  final def compare(that: N): Int = {
    val result: Int =
      if (this.negative != that.negative) 1
      else zip(that).map(lift(_ compare _)).find (_ != 0) getOrElse 0
    signum * result
  }

  final override def equals(other: Any): Boolean =
    // TODO deal with the "erasure" warning; compare numberSystem...
    if (!other.isInstanceOf[N]) false else compare(other.asInstanceOf[N]) == 0
}
