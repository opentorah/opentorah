package org.podval.calendar.numbers

trait NonPeriodicNumberCompanion[S <: Numbers[S], N <: Number[S, N]] extends NumberCompanion[S, N] {
  final override def normalHead(value: Int): Int = value

  protected final override def positiveHead(value: Int): Int = value

  protected final override def negativeHead(value: Int): Int = value
}
