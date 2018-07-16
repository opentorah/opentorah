package org.podval.calendar.numbers

trait PeriodicNumberSystem[S <: PeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Interval <: PeriodicInterval[S]

  def headRange: Int

  require(headRange % 2 == 0)

  protected final override def normalHead(value: Int): Int = normalDigit(value, -1, headRange)._2

  protected final override def positiveHead(value: Int): Int = positiveDigit(value, -1, headRange)._2

  protected final override def negativeHead(value: Int): Int = negativeDigit(value, -1, headRange)._2

  final def symmetrical[N <: PeriodicNumber[S, N]](number: N): Seq[Int] = {
    val result: Seq[Int] = number.normal.digits
    if (result.head <= headRange/2) result
    else (result.head - headRange) +: result.tail
  }

  val period: S#Interval = Interval(headRange)

  val halfPeriod: S#Interval = Interval(headRange/2)
}
