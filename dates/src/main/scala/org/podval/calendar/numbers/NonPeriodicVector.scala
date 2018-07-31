package org.podval.calendar.numbers

trait NonPeriodicVector[S <: NonPeriodicNumbers[S]]
  extends VectorBase[S] with NonPeriodicNumber[S, S#Vector]
{ this: S#Vector =>
  protected final override def normalHead(value: Int): Int = value

  protected final override def positiveHead(value: Int): Int = value

  protected final override def negativeHead(value: Int): Int = value
}
