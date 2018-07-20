package org.podval.calendar.numbers

abstract class Digits(rawDigits: Seq[Int]) {
  def digits: Seq[Int] = if (rawDigits.nonEmpty) rawDigits else Seq(0)
}
