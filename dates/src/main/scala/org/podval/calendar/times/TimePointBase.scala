package org.podval.calendar.times

import org.podval.calendar.numbers.PointBase

trait TimePointBase[S <: Times[S]]
  extends PointBase[S] with Time[S, S#Point]
{ this: S#Point =>
}
