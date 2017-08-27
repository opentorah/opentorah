package org.podval.calendar.numbers

import NumberSystem.{RawNumber, signum}

trait NumberSystem[S <: NumberSystem[S]] { this: S =>

  type Point <: PointBase[S]

  final def newPoint(raw: RawNumber): S#Point = createPoint(normalize(raw))

  protected def createPoint(raw: RawNumber): S#Point

  type Interval <: IntervalBase[S]

  final def newInterval(raw: RawNumber): S#Interval = createInterval(normalize(raw))

  protected def createInterval(raw: RawNumber): S#Interval

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

    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    val resultDigits = newDigits.head +: newDigits.tail.reverse.dropWhile(_ == 0).reverse

    (newNegative, resultDigits)
  }

  def checkHeadDigit(value: Int): Unit

  def correctHeadDigit(value: Int): Int

  final def zipWithRanges(tail: List[Int]): List[(Int, Int)] =
    tail.zipWithIndex.map { case (digit, position) => (digit, range(position)) }

  final def fromRational(value: BigRational, length: Int = defaultLength): RawNumber =
    (value < BigRational.zero, from[BigRational](
      value.abs,
      length,
      _.wholeAndFraction,
      _ * _,
      BigRational.round
    ))

  final def fromDouble(value: Double, length: Int = defaultLength): RawNumber = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      val fraction: Double = what - whole
      (whole.toInt, fraction)
    }

    def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt

    (value < 0.0d, from[Double](
      math.abs(value),
      length,
      wholeAndFraction,
      _ * _,
      round
    ))
  }

  // This is an instance of a specialized unfold with an initiatot, unfolder and terminator
  // (but we don't have even a simple unfold in the standard library)
  private[this] final def from[T](
    value: T,
    length: Int,
    wholeAndFraction: T => (Int, T),
    mult: (T, Int) => T,
    round: (Int, T) => Int): List[Int] =
  {
    val (digits: List[(Int, T)], (lastDigit: Int, lastReminder: T))  =
      (0 until length).toList.map(range)
        .foldLeft((List.empty[(Int, T)], wholeAndFraction(value))) {
          case ((acc, (digit: Int, reminder: T)), range: Int) =>
            (acc :+ (digit, reminder), wholeAndFraction(mult(reminder, range)))
        }
    digits.map(_._1) :+ round(lastDigit, lastReminder)
  }
}


object NumberSystem {
  type RawNumber = (Boolean, List[Int])

  final def signum(negative: Boolean): Int = if (negative) -1 else +1
}
