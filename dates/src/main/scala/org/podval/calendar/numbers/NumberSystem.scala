package org.podval.calendar.numbers

import Convertible.ConvertibleOps

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  def createPoint(digits: Seq[Int]): Point

  val Point: PointCompanion[S]

  type Vector <: VectorBase[S]

  def createVector(digits: Seq[Int]): Vector

  val Vector: VectorCompanion[S]

  val defaultLength: Int

  /**
    *
    * @param position within the tail
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSign: String

  val signPartial: PartialFunction[Int, String]

  final def to[T: Convertible](digits: Seq[Int]): T = NumberSystem.to[T](digits, ranges(digits.tail.length))

  final def from[T : Convertible](value: T, length: Int): Seq[Int] = NumberSystem.from[T](value, ranges(length))

  private def ranges(length: Int): Seq[Int] = (0 until length).map(range)

  // TODO move into companion object; pass signs in
  // TODO tests with negative digits - and for angles
  final def toString[N <: Number[S, N]](number: N, length: Int): String = {
    def signFor(position: Int): Option[String] = signPartial.lift(position)

    def digitToString(defaultSign: String)(pair: (Int, Option[String])): String = {
      val (digit: Int, sign: Option[String]) = pair
      math.abs(digit) + sign.getOrElse(defaultSign)
    }

    val digits: Seq[Int] = number.digits
    val digitsWithSigns: Seq[(Int, Option[String])] = digits.tail.padTo(length, 0).zipWithIndex.map {
      case (digit, position) => (digit, signFor(position))
    }
    val tailResult: Seq[String] =
      if (digitsWithSigns.isEmpty) Seq.empty
      else digitsWithSigns.init.map(digitToString(",")) :+ digitToString("")(digitsWithSigns.last)

    val result: Seq[String] = (math.abs(digits.head) + headSign) +: tailResult

    (if (number.isNegative) "-" else "") + result.mkString
  }

  // TODO normalize and chop off trailing zeroes when constructing
  final def normal[N <: Number[S, N]](number: N): Seq[Int] = transform(number.digits, normalDigit, normalHead)

  protected final def normalDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    (digit / digitRange, digit % digitRange)

  protected def normalHead(value: Int): Int

  def withSign(isPositive: Boolean, digits: Seq[Int]): Seq[Int] = transform(digits,
    if (isPositive) positiveDigit else negativeDigit,
    if (isPositive) positiveHead else negativeHead
  )

  protected final def positiveDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit >= 0) (0, digit) else (-1, digit + digitRange)

  protected def positiveHead(value: Int): Int

  protected final def negativeDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit <= 0) (0, digit) else (1, digit - digitRange)

  protected def negativeHead(value: Int): Int

  final def transform(
    digits: Seq[Int],
    forDigit: (Int, Int, Int) => (Int, Int),
    forHead: Int => Int
  ): Seq[Int] = {
    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex :\(0, Seq.empty[Int])) {
      case ((digit: Int, position: Int), (carry: Int, result: Seq[Int])) =>
        val (resultCarry, resultDigit) = forDigit(digit + carry, position, range(position))
        (resultCarry, resultDigit +: result)
    }

    forHead(digits.head + headCarry) +: newTail
  }
}

object NumberSystem {
  def to[T: Convertible](digits: Seq[Int], ranges: Seq[Int]): T = {
    digits.zip(ranges :+ 0).foldLeft((Convertible[T].zero, BigInt(1))) {
      case ((acc: T, denominator: BigInt), (digit: Int, range: Int)) =>
        (acc + Convertible[T].div(digit, denominator), denominator*range)
    }._1
  }

  def from[T : Convertible](value: T, ranges: Seq[Int]): Seq[Int] = {
    val (digits: Seq[Int], lastValue: T) = ranges.foldLeft((Seq.empty[Int], value.abs)) {
      case ((acc: Seq[Int], value: T), range: Int) => (acc :+ value.whole, value.fraction * range)
    }

    (digits :+ lastValue.round).map(value.signum*_)
  }
}
