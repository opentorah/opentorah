package org.podval.calendar.numbers

trait NonPeriodicNumbers[S <: NonPeriodicNumbers[S]] extends Numbers[S] { this: S =>
  type Point <: NonPeriodicPoint[S]

  type Vector <: NonPeriodicVector[S]

  final override def headRangeOpt: Option[Int] = None
}
