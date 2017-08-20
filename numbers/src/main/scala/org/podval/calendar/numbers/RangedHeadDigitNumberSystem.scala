package org.podval.calendar.numbers

trait RangedHeadDigitNumberSystem[S <: NumberSystem[S]] extends NumberSystem[S] { this: S =>
  def headRange: Int

  final override def checkHeadDigit(value: Int): Unit =
    require(value < headRange, "must be less than " + headRange)

  final override def correctHeadDigit(value: Int): Int = {
    val result = value % headRange
    if (value >= 0) result else result + headRange
  }
}
