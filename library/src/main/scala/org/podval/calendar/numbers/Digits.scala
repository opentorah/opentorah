package org.podval.calendar.numbers

abstract class Digits(val digits: Seq[Int]) {
  // at leastthe head digit is present
  require(digits.nonEmpty)
  // no trailing 0s
  require(!digits.tail.lastOption.contains(0))
}
