package org.podval.calendar.numbers

trait PeriodicNumbers[S <: PeriodicNumbers[S]] extends Numbers[S] { this: S =>
  override type Point <: PeriodicPoint[S]

  override type Vector <: PeriodicVector[S]

  override type VectorCompanionType <: VectorCompanion[S]

  final override def headRangeOpt: Option[Int] = Some(headRange)

  def headRange: Int

  require(headRange % 2 == 0)

  val period: S#Vector = Vector(headRange)

  val halfPeriod: S#Vector = Vector(headRange/2)
}
