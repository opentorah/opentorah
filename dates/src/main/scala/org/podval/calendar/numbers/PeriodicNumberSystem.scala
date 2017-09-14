package org.podval.calendar.numbers

trait PeriodicNumberSystem[S <: PeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Interval <: PeriodicInterval[S]

  def headRange: Int

  final override def canonical(digits: Seq[Int]): Seq[Int] = {
    val result: Seq[Int] = normal(digits)
    if (isNegative(result)) complement(result) else result
  }

  final def complement(digits: Seq[Int]): Seq[Int] =
    (headRange - digits.head) +: zipWithRanges(digits).map { case (digit, range) => range - digit }

  final override def correctHeadDigit(value: Int): Int = {
    val result = value % headRange
    if (result >= 0) result else result+headRange
  }

  require(headRange % 2 == 0)

  val period: S#Interval = Interval(headRange)
}
