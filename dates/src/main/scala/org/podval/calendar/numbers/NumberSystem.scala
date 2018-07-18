package org.podval.calendar.numbers

import Convertible.ConvertibleOps

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  def createPoint(digits: Seq[Int]): Point

  val Point: PointCompanion[S]

  type Interval <: IntervalBase[S]

  def createInterval(digits: Seq[Int]): Interval

  val Interval: IntervalCompanion[S]

  val defaultLength: Int

  /**
    *
    * @param position within the tail
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSign: String

  val signPartial: PartialFunction[Int, String]

  // TODO comments
  final def to[T: Convertible](digits: Seq[Int]): T = {
    val ev: Convertible[T] = Convertible[T]

    val zeroDenominator: BigInt = BigInt(1)
    digits.tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }
    .foldLeft[(T, BigInt)]((ev.div(digits.head, zeroDenominator), zeroDenominator)) {
      case ((acc: T, denominator: BigInt), (digit: Int, range: Int)) =>
        val newDenominator: BigInt = denominator*range
        (acc + ev.div(digit, newDenominator), newDenominator)
    }._1
  }

  // TODO comments
  // This is an instance of a specialized unfold with an initiator, unfolder and terminator
  // (but we don't have even a simple unfold in the standard library)
  final def from[T : Convertible](value: T, length: Int): Seq[Int] = {
    def wholeAndFraction(value: T): (Int, T) = (value.whole, value.fraction)

    val (rawDigits: Seq[(Int, T)], (lastDigit: Int, lastReminder: T))  = (0 until length).map(range)
      .foldLeft((Seq.empty[(Int, T)], wholeAndFraction(value.abs))) {
        case ((acc, (digit: Int, reminder: T)), range: Int) =>
          (acc :+ (digit, reminder), wholeAndFraction(reminder * range))
      }

    val digits: Seq[Int] = rawDigits.map(_._1)
    val result: Seq[Int] = digits :+ (lastDigit + lastReminder.round)
    result.map(value.signum*_)
  }

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
