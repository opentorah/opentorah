package org.podval.calendar.numbers

trait NotRangedHeadDigitNumberSystem[S <: NumberSystem[S]] extends NumberSystem[S] { this: S =>
  final override def checkHeadDigit(value: Int): Unit = {}

  final override def correctHeadDigit(value: Int): Int = value
}
