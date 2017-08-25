package org.podval.calendar.numbers

import NumberSystem.{RawNumber, signum}

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  final def newPoint(raw: RawNumber): S#Point = createPoint(normalize(raw))

  protected def createPoint(raw: RawNumber): S#Point

  type Interval <: IntervalBase[S]

  final def newInterval(raw: RawNumber): S#Interval = createInterval(normalize(raw))

  protected def createInterval(raw: RawNumber): S#Interval

  /**
    *
    * @param position within the tail
    * @return positive, even number
    */
  def range(position: Int): Int

  def headSign: String

  val signPartial: PartialFunction[Int, String]

  def sign(position: Int): Option[String] = signPartial.lift(position)

  /**
    *
    * @param position is from the head (where it is 1); first digit in the tail is position 1
    * @return
    */
  // TODO where I need to divide by a multiplier, I convert it to a long (with possible
  // ArithmeticException) - because I do not know how to divide by a BigInt :)
  // TODO remove multiplier and ensure that ranges are multiplied as BigInts
  final def multiplier(position: Int): BigInt =
    (1 to position).map(position => BigInt(range(position-1))).product

  private final def normalize(raw: RawNumber): RawNumber = {
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

    // Drop trailing zeros; use reverse() since there is no dropWhileRight :)
    val resultDigits = newDigits.reverse.dropWhile(_ == 0).reverse

    (newNegative, resultDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  final def zipWithRanges(tail: List[Int]): List[(Int, Int)] =
    tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }

  final def fromRational(value: BigRational, length: Int): RawNumber = {
    def wholeAndFraction(what: BigRational): (Int, BigRational) =
      what.wholeAndFraction

    def round(what: (Int, BigRational)): Int = BigRational.round(what)

    val negative = value.negative

    val (digits: List[(Int, BigRational)], lastDigit: (Int, BigRational))  =
      (0 until length).toList.map(range)
      .foldLeft((List.empty[(Int, BigRational)], wholeAndFraction(value))) {
        case ((acc, (digit: Int, reminder: BigRational)), range: Int) =>
          (acc :+ (digit, reminder), wholeAndFraction(reminder*range))
      }
    (negative, digits.map(_._1) :+ round(lastDigit))
  }

  final def fromDouble(signedValue: Double, length: Int): RawNumber = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      val fraction: Double = what - whole
      (whole.toInt, fraction)
    }

    def round(what: (Int, Double)): Int = what._1 + math.round(what._2).toInt

    val negative: Boolean = signedValue < 0
    val value: Double = signum(negative) * signedValue

    val (digits: List[(Int, Double)], lastDigit: (Int, Double))  =
      (0 until length).toList.map(range)
      .foldLeft((List.empty[(Int, Double)], wholeAndFraction(value))) {
        case ((acc, (digit: Int, reminder: Double)), range: Int) =>
          (acc :+ (digit, reminder), wholeAndFraction(reminder*range))
      }
    (negative, digits.map(_._1) :+ round(lastDigit))
  }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])

  final def signum(negative: Boolean): Int = if (negative) -1 else +1
}
