package org.podval.calendar.numbers

trait PeriodicVectorCompanion[S <: PeriodicNumbers[S]] extends VectorCompanion[S] {
  final override def normalHead(value: Int): Int = headDigit(normalDigit, value)

  protected final override def positiveHead(value: Int): Int = headDigit(positiveDigit, value)

  protected final override def negativeHead(value: Int): Int = headDigit(negativeDigit, value)

  private def headDigit(f: (Int, Int, Int) => (Int, Int), value: Int): Int = f(value, -1, numbers.headRange)._2
}
