package org.podval.calendar.dates


trait NotRangedHeadDigitNumberSystem extends NumberSystem {

  final override def checkHeadDigit(value: Int) = {}


  final override def correctHeadDigit(value: Int): Int = value
}
