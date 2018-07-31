package org.podval.calendar.times

import org.podval.calendar.numbers.NonPeriodicPoint

trait TimePointBase[S <: Times[S]]
  extends NonPeriodicPoint[S] with Time[S, S#Point]
{ this: S#Point =>
}
