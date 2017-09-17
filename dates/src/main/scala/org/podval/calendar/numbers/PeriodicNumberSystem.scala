package org.podval.calendar.numbers

trait PeriodicNumberSystem[S <: PeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Interval <: PeriodicInterval[S]

  def headRange: Int

  require(headRange % 2 == 0)

  protected final override def normalHead(value: Int): Int = normalDigit(value, headRange)._2

  protected final override def positiveHead(value: Int): Int = positiveDigit(value, headRange)._2

  protected final override def negativeHead(value: Int): Int = negativeDigit(value, headRange)._2

  // TODO test
  final def symmetrical(digits: Seq[Int]): Seq[Int] = {
    val result: Seq[Int] = normal(digits)
    if (result.head <= headRange/2) result
    else (result.head - headRange) +: result.tail
  }

  val period: S#Interval = Interval(headRange)
}
