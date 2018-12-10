package org.podval.calendar.numbers

import Convertible.ConvertibleOps

trait Numbers[S <: Numbers[S]] { this: S =>

  type Point <: PointBase[S]

  val Point: PointCompanion[S]

  type Vector <: VectorBase[S]

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

  final def to[T: Convertible](digits: Seq[Int]): T =
    digits.zip(ranges(digits.length-1) :+ 0).foldLeft((Convertible[T].zero, BigInt(1))) {
      case ((acc, denominator: BigInt), (digit: Int, range: Int)) =>
        (acc + Convertible[T].div(digit, denominator), denominator*range)
    }._1

  final def from[T : Convertible](value: T, length: Int): Seq[Int] = {
    val (digits: Seq[Int], lastValue) = ranges(length).foldLeft((Seq.empty[Int], value.abs)) {
      case ((acc: Seq[Int], v), range: Int) => (acc :+ v.whole, v.fraction * range)
    }

    (digits :+ lastValue.round).map(value.signum*_)
  }

  private def ranges(length: Int): Seq[Int] = (0 until length).map(range)

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
