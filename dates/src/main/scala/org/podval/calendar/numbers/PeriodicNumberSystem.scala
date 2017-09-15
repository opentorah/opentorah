package org.podval.calendar.numbers

trait PeriodicNumberSystem[S <: PeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Interval <: PeriodicInterval[S]

  def headRange: Int

  require(headRange % 2 == 0)

  final override def normalHead(value: Int): Int = {
    val result = value % headRange
    if (result >= 0) result else result+headRange
  }

  final def symmetrical(digits: Seq[Int]): Seq[Int] = {
    val result: Seq[Int] = normal(digits)
    if (result.head <= headRange/2) result
    else (result.head - headRange) +: result.tail
  }

  val period: S#Interval = Interval(headRange)
}
