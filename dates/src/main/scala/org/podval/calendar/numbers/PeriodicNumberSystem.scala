package org.podval.calendar.numbers

trait PeriodicNumberSystem[S <: PeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Interval <: PeriodicInterval[S]

  def headRange: Int

  require(headRange % 2 == 0)

  val period: S#Interval = Interval(headRange)
}
