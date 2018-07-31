package org.podval.calendar.numbers

trait NonPeriodicPoint[S <: NonPeriodicNumbers[S]]
  extends PointBase[S] with NonPeriodicNumber[S, S#Point]
{ this: S#Point =>
  protected final override def normalHead(value: Int): Int = value

  protected final override def positiveHead(value: Int): Int = value

  protected final override def negativeHead(value: Int): Int = value
}
