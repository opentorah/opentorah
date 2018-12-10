package org.podval.calendar.numbers

trait NonPeriodicPoint[S <: NonPeriodicNumbers[S]]
  extends PointBase[S] with NonPeriodicNumber[S, S#Point]
{ this: S#Point =>
}
