package org.podval.calendar.numbers

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]] extends NumberSystemMember[S] {
  def apply(digits: Int*): N

  final def fromDigits(digits: Seq[Int]): N = apply(digits: _*)

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N =
    fromDigits(from[BigRational](
      value.signum,
      value.abs,
      length,
      _.wholeAndFraction,
      _ * _,
      BigRational.round
    ))

  final def fromDouble(value: Double, length: Int = numberSystem.defaultLength): N = {
    def wholeAndFraction(what: Double): (Int, Double) = {
      val whole: Double = math.floor(what)
      val fraction: Double = what - whole
      (whole.toInt, fraction)
    }

    def round(whole: Int, fraction: Double): Int = whole + math.round(fraction).toInt

    fromDigits(from[Double](
      math.signum(value).toInt,
      math.abs(value),
      length,
      wholeAndFraction,
      _ * _,
      round
    ))
  }

  // This is an instance of a specialized unfold with an initiator, unfolder and terminator
  // (but we don't have even a simple unfold in the standard library)
  private[this] final def from[T](
    signum: Int,
    value: T,
    length: Int,
    wholeAndFraction: T => (Int, T),
    mult: (T, Int) => T,
    round: (Int, T) => Int): Seq[Int] =
  {
    val (rawDigits: Seq[(Int, T)], (lastDigit: Int, lastReminder: T))  =
      (0 until length).map(numberSystem.range)
        .foldLeft((Seq.empty[(Int, T)], wholeAndFraction(value))) {
          case ((acc, (digit: Int, reminder: T)), range: Int) =>
            (acc :+ (digit, reminder), wholeAndFraction(mult(reminder, range)))
        }
    val digits: Seq[Int] = rawDigits.map(_._1)
    signum*digits.head +: digits.tail :+ round(lastDigit, lastReminder)
  }

  protected final def normalize(rawDigits: Seq[Int]): Seq[Int] = {
    def step(elem: (Int, Int), acc: (Int, Seq[Int])) = {
      val (digit, position) = elem
      val (carry, result) = acc
      val value: Int = digit + carry
      val range: Int = numberSystem.range(position)
      val (quotient: Int, reminder: Int) = (value / range, value % range)
      val (resultCarry, resultDigit) =
        if (value >= 0) (quotient, reminder)
        else (quotient - 1, reminder + range)
      (resultCarry, resultDigit +: result)
    }

    val digits: Seq[Int] = if (rawDigits.nonEmpty) rawDigits else Seq(0)

    val (headCarry: Int, newTail: Seq[Int]) = (digits.tail.zipWithIndex :\(0, Seq.empty[Int]))(step)
    val newHead: Int = digits.head + headCarry

    // Drop trailing zeros in the tail; use reverse() since there is no dropWhileRight :)
    newHead +: newTail.reverse.dropWhile(_ == 0).reverse
  }
}
