package org.podval.calendar.dates


trait RangedHeadDigitNumberSystem extends NumberSystem {

  def headRange: Int


  final override def checkHeadDigit(value: Int) = require(value < headRange, "must be less than " + headRange)


  final override def correctHeadDigit(value: Int): Int = {
    val result = value % headRange
    if (value >= 0) result else result + headRange
  }
}
