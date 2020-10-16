package org.opentorah.times

import org.opentorah.numbers.{Digits, PointNumber}

abstract class TimePointBase[S <: Times[S]](numbers: S, digits: Digits)
  extends PointNumber[S](numbers, digits) with Time[S, S#Point]
{ this: S#Point =>
}
