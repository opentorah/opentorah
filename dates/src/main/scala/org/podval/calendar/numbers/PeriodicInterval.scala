package org.podval.calendar.numbers

abstract class PeriodicInterval[S <: PeriodicNumberSystem[S]](negative: Boolean, digits: Seq[Int])
  extends IntervalBase[S](negative, digits) with PeriodicNumber[S, S#Interval]
{ this: S#Interval =>
}
