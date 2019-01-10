package org.podval.calendar.numbers

trait NumberCompanion[S <: Numbers[S], N <: Number[S, N]] extends NumbersMember[S] {
  final val zero: N = apply(0)

  def apply(digits: Int*): N

  final def fromDigits(digits: Seq[Int]): N = apply(digits: _*)

  final def withSign(isPositive: Boolean, digits: Seq[Int]): Seq[Int] = numbers.transform(digits,
    if (isPositive) positiveDigit else negativeDigit,
    if (isPositive) positiveHead else negativeHead
  )

  final def normalDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    (digit / digitRange, digit % digitRange)

  def normalHead(value: Int): Int

  final def positiveDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit >= 0) (0, digit) else (-1, digit + digitRange)

  protected def positiveHead(value: Int): Int

  final def negativeDigit(digit: Int, position: Int, digitRange: Int): (Int, Int) =
    if (digit <= 0) (0, digit) else (1, digit - digitRange)

  protected def negativeHead(value: Int): Int

  final def from[T: Convertible](value: T, length: Int): N =
    fromDigits(numbers.from[T](value, length))

  final def fromRational(value: BigRational, length: Int): N = from[BigRational](value, length)

  final def fromDouble(value: Double, length: Int): N = from[Double](value, length)
}
