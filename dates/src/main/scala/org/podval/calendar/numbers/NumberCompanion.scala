package org.podval.calendar.numbers

trait NumberCompanion[S <: Numbers[S], N <: Number[S, N]] extends NumbersMember[S] {
  val zero: N = apply(0)

  def apply(digits: Int*): N

  final def fromDigits(digits: Seq[Int]): N = apply(digits: _*)

  final def from[T: Convertible](value: T, length: Int = numberSystem.defaultLength): N =
    fromDigits(numberSystem.from[T](value, length))

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N = from[BigRational](value, length)
  final def fromDouble(value: Double, length: Int = numberSystem.defaultLength): N = from[Double](value, length)
}
