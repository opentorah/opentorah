package org.opentorah.times

import org.opentorah.numbers.{Digits, VectorNumber}

abstract class TimeVectorBase[S <: Times[S]](digits: Digits)
  extends VectorNumber[S](digits) with Time[S, S#Vector]
{ this: S#Vector =>
}
