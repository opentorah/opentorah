package org.podval.calendar.numbers

trait NonPeriodicNumber[S <: NonPeriodicNumbers[S], N <: NonPeriodicNumber[S, N]]
  extends Number[S, N]
{ this: N =>
}
