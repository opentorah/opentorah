package org.podval.calendar.numbers

trait RangedHeadDigitNumberSystem[S <: NumberSystem[S]] extends NumberSystem[S] { this: S =>
  def headRange: Int

  final override def checkHeadDigit(value: Int): Unit =
    require(value < headRange, "must be less than " + headRange)

  final override def correctHeadDigit(value: Int): Int = {
    val result = value % headRange
    if (value >= 0) result else result + headRange
  }

  // TODO add operations to convert a number to/from positive/minimal representation.

  // TODO tweak equals() so that -15 degrees and +345 degrees are equals!
}
