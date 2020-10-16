package org.opentorah.numbers

abstract class NumberCompanion[S <: Numbers[S], N <: Number[S, N]](numbers: S)
  extends NumbersMember[S](numbers)
{
  final lazy val zero: N = apply(0)

  final def apply(digits: Int*): N = fromDigits(digits)

  final def fromRational(value: BigRational, length: Int): N = from[BigRational](value, length)

  final def fromDouble(value: Double, length: Int): N = from[Double](value, length)

  private final def from[T: Convertible](value: T, length: Int): N = fromDigits(numbers.from[T](value, length))

  final def fromDigits(digits: Digits): N = newNumber(numbers.normalize(digits, isCanonical = isCanonical))

  protected def newNumber(digits: Digits): N

  protected def isCanonical: Boolean
}
