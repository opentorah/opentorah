package org.podval.calendar.numbers

abstract class PeriodicInterval[S <: PeriodicNumberSystem[S]](digits: Seq[Int])
  extends IntervalBase[S](digits) with PeriodicNumber[S, S#Interval]
{ this: S#Interval =>
}
