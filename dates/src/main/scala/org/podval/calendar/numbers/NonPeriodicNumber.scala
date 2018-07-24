package org.podval.calendar.numbers

trait NonPeriodicNumber[S <: NonPeriodicNumbers[S], N <: NonPeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>

  protected final override def normalHead(value: Int): Int = value

  protected final override def positiveHead(value: Int): Int = value

  protected final override def negativeHead(value: Int): Int = value
}
