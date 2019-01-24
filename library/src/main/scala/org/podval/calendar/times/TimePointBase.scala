package org.podval.calendar.times

import org.podval.calendar.numbers.PointNumber

trait TimePointBase[S <: Times[S]]
  extends PointNumber[S] with Time[S, S#Point]
{ this: S#Point =>
}
