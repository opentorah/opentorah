package org.podval.calendar.numbers

trait PeriodicNumbers[S <: PeriodicNumbers[S]] extends Numbers[S] { this: S =>
  type Point <: PeriodicPoint[S]

  type Vector <: PeriodicVector[S]

  override type VectorCompanionType <: PeriodicVectorCompanion[S]

  def headRange: Int

  require(headRange % 2 == 0)

  protected final override def headDigit(f: (Int, Int, Int) => (Int, Int), value: Int): Int = f(value, -1, headRange)._2

  val period: S#Vector = Vector(headRange)

  val halfPeriod: S#Vector = Vector(headRange/2)
}
