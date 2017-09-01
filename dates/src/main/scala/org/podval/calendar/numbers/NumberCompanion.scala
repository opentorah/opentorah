package org.podval.calendar.numbers

import NumberSystem.RawNumber

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]]
  extends NumberSystemMember[S]
{
  def newNumber(raw: RawNumber): N

  final def apply(raw: RawNumber): N = newNumber(raw)

  final def apply(negative: Boolean, digits: Int*): N =
    apply((negative, (if (digits.nonEmpty) digits else Seq(0)).toList))

  final def apply(digits: Int*): N = apply(negative = false, digits: _*)

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N =
    newNumber(value < BigRational.zero, from[BigRational](
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

    newNumber(value < 0.0d, from[Double](
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
    value: T,
    length: Int,
    wholeAndFraction: T => (Int, T),
    mult: (T, Int) => T,
    round: (Int, T) => Int): List[Int] =
  {
    val (digits: List[(Int, T)], (lastDigit: Int, lastReminder: T))  =
      (0 until length).toList.map(numberSystem.range)
        .foldLeft((List.empty[(Int, T)], wholeAndFraction(value))) {
          case ((acc, (digit: Int, reminder: T)), range: Int) =>
            (acc :+ (digit, reminder), wholeAndFraction(mult(reminder, range)))
        }
    digits.map(_._1) :+ round(lastDigit, lastReminder)
  }
}
