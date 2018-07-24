package org.podval.calendar.numbers

// TODO introduce NonPeriodicPoint/Vector - or remove NonPeriodicNumbers...
trait NonPeriodicNumbers[S <: NonPeriodicNumbers[S]] extends Numbers[S] { this: S =>
  type Point <: PointBase[S] with NonPeriodicNumber[S, Point]

  type Vector <: VectorBase[S] with NonPeriodicNumber[S, Point]
}
