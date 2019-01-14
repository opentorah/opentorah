package org.podval.calendar.numbers

trait NonPeriodicNumbers[S <: NonPeriodicNumbers[S]] extends Numbers[S] { this: S =>
  type Point <: NonPeriodicPoint[S]

  type Vector <: NonPeriodicVector[S]

  protected final override def headDigit(f: (Int, Int, Int) => (Int, Int), value: Int): Int = value
}
