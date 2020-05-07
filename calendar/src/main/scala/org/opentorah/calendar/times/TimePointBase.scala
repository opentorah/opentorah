package org.opentorah.calendar.times

import org.opentorah.numbers.PointNumber

trait TimePointBase[S <: Times[S]]
  extends PointNumber[S] with Time[S, S#Point]
{ this: S#Point =>
}
