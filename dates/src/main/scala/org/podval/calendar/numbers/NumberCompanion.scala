package org.podval.calendar.numbers

import NumberSystem.{RawNumber, signum}

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]] extends NumberSystemMember[S] {
  def apply(raw: RawNumber): N

  final def apply(negative: Boolean, digits: Int*): N =
    apply((negative, if (digits.nonEmpty) digits else Seq(0)))

  final def apply(digits: Int*): N = apply(negative = false, digits: _*)

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N =
    apply((value < BigRational.zero, from[BigRational](
      value.abs,
      length,
      _.wholeAndFraction,
      _ * _,
      BigRational.round
    )))

  final def fromDouble(value: Double, length: Int = numberSystem.defaultLength): N = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      val fraction: Double = what - whole
      (whole.toInt, fraction)
    }

    def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt

    apply((value < 0.0d, from[Double](
      math.abs(value),
      length,
      wholeAndFraction,
      _ * _,
      round
    )))
  }

  // This is an instance of a specialized unfold with an initiator, unfolder and terminator
  // (but we don't have even a simple unfold in the standard library)
  private[this] final def from[T](
    value: T,
    length: Int,
    wholeAndFraction: T => (Int, T),
    mult: (T, Int) => T,
    round: (Int, T) => Int): Seq[Int] =
  {
    val (digits: Seq[(Int, T)], (lastDigit: Int, lastReminder: T))  =
      (0 until length).map(numberSystem.range)
        .foldLeft((Seq.empty[(Int, T)], wholeAndFraction(value))) {
          case ((acc, (digit: Int, reminder: T)), range: Int) =>
            (acc :+ (digit, reminder), wholeAndFraction(mult(reminder, range)))
        }
    digits.map(_._1) :+ round(lastDigit, lastReminder)
  }

  protected final def normalize(raw: RawNumber): RawNumber = {
    def step(elem: (Int, Int), acc: (Int, Seq[Int])) = {
      val (digit, position) = elem
      val (carry, result) = acc
      val value: Int = digit + carry
      val range: Int = numberSystem.range(position)
      val (quotient: Int, reminder: Int) = (value / range, value % range)
      val (carry_, digit_) =
        if (value >= 0) (quotient, reminder)
        else (quotient - 1, reminder + range)
      (carry_, digit_ +: result)
    }

    def headStep(head: Int, headCarry: Int): (Boolean, Int) = {
      val carriedHead: Int = numberSystem.correctHeadDigit(head + headCarry)
      val carriedNegative: Boolean = carriedHead < 0
      (carriedNegative, signum(carriedNegative) * carriedHead)
    }

    val (negative: Boolean, digits: Seq[Int]) = raw
    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex :\(0, Seq.empty[Int]))(step)
    val (carriedNegative: Boolean, newHead: Int) = headStep(digits.head, headCarry)

    val newNegative: Boolean = if (negative) !carriedNegative else carriedNegative
    val newDigits = newHead +: newTail

    // Ensure that digits are within appropriate ranges
    newDigits.foreach(digit => require(digit >= 0, s"$digit must be non-negative"))
    numberSystem.checkHeadDigit(newHead)
    numberSystem.zipWithRanges(newTail).foreach
    { case (digit, range) => require(digit < range, s"$digit must be less than $range") }

    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    val resultDigits = newDigits.head +: newDigits.tail.reverse.dropWhile(_ == 0).reverse
    // Treat -0 as 0
    val resultNegative = if ((newDigits.length == 1) && (newDigits.head == 0)) false else newNegative
    (resultNegative, resultDigits)
  }
}
