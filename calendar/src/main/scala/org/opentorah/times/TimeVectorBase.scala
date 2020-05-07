package org.opentorah.times

import org.opentorah.numbers.VectorNumber

trait TimeVectorBase[S <: Times[S]]
  extends VectorNumber[S] with Time[S, S#Vector]
{ this: S#Vector =>
}
