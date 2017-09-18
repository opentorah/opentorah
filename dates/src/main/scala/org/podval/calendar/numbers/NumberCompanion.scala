package org.podval.calendar.numbers

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]] extends NumberSystemMember[S] {
  val zero: N = apply(0)

  def apply(digits: Int*): N

  final def fromDigits(digits: Seq[Int]): N = apply(digits: _*)

  final def fromRational(value: BigRational, length: Int = numberSystem.defaultLength): N =
    fromDigits(numberSystem.fromRational(value, length))

  final def fromDouble(value: Double, length: Int = numberSystem.defaultLength): N =
    fromDigits(numberSystem.fromDouble(value, length))
}
