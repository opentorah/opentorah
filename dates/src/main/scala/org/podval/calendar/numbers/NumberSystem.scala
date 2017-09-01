package org.podval.calendar.numbers

import NumberSystem.{RawNumber, signum}

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  // TODO now that it is accessible, verify the input?
  def createPoint(raw: RawNumber): S#Point

  val Point: PointCompanion[S]

  type Interval <: IntervalBase[S]

  def createInterval(raw: RawNumber): S#Interval

  val Interval: IntervalCompanion[S]

  // TODO use it for all operations, including + and -, to guide which overflow digit to round.
  val defaultLength: Int

  /**
    *
    * @param position within the tail
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSign: String

  val signPartial: PartialFunction[Int, String]

  def sign(position: Int): Option[String] = signPartial.lift(position)

  // TODO move into NumberCompanion
  final def normalize(raw: RawNumber): RawNumber = {
    def step(elem: (Int, Int), acc: (Int, List[Int])) = {
      val (digit, position) = elem
      val (carry, result) = acc
      val value: Int = digit + carry
      val range: Int = this.range(position)
      val (quotient: Int, reminder: Int) = (value / range, value % range)
      val (carry_, digit_) =
        if (value >= 0) (quotient, reminder)
        else (quotient - 1, reminder + range)
      (carry_, digit_ :: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val carriedHead: Int = correctHeadDigit(head + headCarry)
      val carriedNegative: Boolean = carriedHead < 0
      (carriedNegative, signum(carriedNegative) * carriedHead)
    }

    val (negative: Boolean, digits: List[Int]) = raw
    val (headCarry: Int, newTail: List[Int]) = (digits.tail.zipWithIndex :\(0, List.empty[Int]))(step)
    val (carriedNegative: Boolean, newHead: Int) = headStep(digits.head, headCarry)

    val newNegative: Boolean = if (negative) !carriedNegative else carriedNegative
    val newDigits = newHead :: newTail

    // Ensure that digits are within appropriate ranges
    newDigits.foreach(digit => require(digit >= 0, s"$digit must be non-negative"))
    checkHeadDigit(newHead)
    zipWithRanges(newTail).foreach
      { case (digit, range) => require(digit < range, s"$digit must be less than $range") }

    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    val resultDigits = newDigits.head +: newDigits.tail.reverse.dropWhile(_ == 0).reverse
    // Treat -0 as 0
    val resultNegative = if ((newDigits.length == 1) && (newDigits.head == 0)) false else newNegative
    (resultNegative, resultDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  final def zipWithRanges(tail: List[Int]): List[(Int, Int)] =
    tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])

  final def signum(negative: Boolean): Int = if (negative) -1 else +1
}
