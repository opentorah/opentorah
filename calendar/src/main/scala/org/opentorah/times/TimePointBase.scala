package org.opentorah.times

import org.opentorah.numbers.{Digits, PointNumber}

abstract class TimePointBase[S <: Times[S]](digits: Digits)
  extends PointNumber[S](digits) with Time[S, S#Point]
{ this: S#Point =>
}
