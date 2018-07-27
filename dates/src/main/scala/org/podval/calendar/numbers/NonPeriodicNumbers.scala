package org.podval.calendar.numbers

// TODO introduce NonPeriodicPoint/Vector - or remove NonPeriodicNumbers...
trait NonPeriodicNumbers[S <: NonPeriodicNumbers[S]] extends Numbers[S] { this: S =>
  type Point <: PointBase[S] // TODO with NonPeriodicNumber[S, Point]

  type Vector <: VectorBase[S] // TODO with NonPeriodicNumber[S, Vector]
}
