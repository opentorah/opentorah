package org.podval.calendar.numbers

trait NonPeriodicNumberSystem[S <: NonPeriodicNumberSystem[S]] extends NumberSystem[S] { this: S =>
  type Point <: PointBase[S]

  type Interval <: IntervalBase[S]

  protected final override def normalHead(value: Int): Int = value

  protected final override def positiveHead(value: Int): Int = value

  protected final override def negativeHead(value: Int): Int = value
}
