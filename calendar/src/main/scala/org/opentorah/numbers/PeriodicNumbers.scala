package org.opentorah.numbers

trait PeriodicNumbers[S <: PeriodicNumbers[S]] extends Numbers[S] { this: S =>
  final override def headRangeOpt: Option[Int] = Some(headRange)

  def headRange: Int

  require(headRange % 2 == 0)

  final lazy val period: S#Vector = Vector(headRange)

  final lazy val halfPeriod: S#Vector = Vector(headRange/2)
}
