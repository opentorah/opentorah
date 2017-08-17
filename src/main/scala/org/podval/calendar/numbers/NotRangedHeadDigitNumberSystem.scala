package org.podval.calendar.numbers

trait NotRangedHeadDigitNumberSystem extends NumberSystem {
  final override def checkHeadDigit(value: Int): Unit = {}

  final override def correctHeadDigit(value: Int): Int = value
}
