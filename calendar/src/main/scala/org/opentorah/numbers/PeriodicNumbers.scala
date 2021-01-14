package org.opentorah.numbers

trait PeriodicNumbers extends Numbers {
  final override def headRangeOpt: Option[Int] = Some(headRange)

  def headRange: Int

  require(headRange % 2 == 0)

  final lazy val period: Vector = Vector(headRange)

  final lazy val halfPeriod: Vector = Vector(headRange/2)
}
