package org.podval.calendar.numbers

trait PeriodicVector[S <: PeriodicNumbers[S]]
  extends VectorBase[S] with PeriodicNumber[S, S#Vector]
{ this: S#Vector =>
  // TODO #56: Vector shouldn't be periodic!
  protected final override def normalHead(value: Int): Int = headDigit(normalDigit, value)

  protected final override def positiveHead(value: Int): Int = headDigit(positiveDigit, value)

  protected final override def negativeHead(value: Int): Int = headDigit(negativeDigit, value)

  private def headDigit(f: (Int, Int, Int) => (Int, Int), value: Int): Int = f(value, -1, numbers.headRange)._2

//  protected final override def normalHead(value: Int): Int = value
//
//  protected final override def positiveHead(value: Int): Int = value
//
//  protected final override def negativeHead(value: Int): Int = value
}
