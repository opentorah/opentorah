package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class RangedHeadDigitPoint[S <: RangedHeadDigitNumberSystem[S]](raw: RawNumber)
  extends PointBase[S](raw) with RangedHeadDigitNumber[S, S#Point]
{ this: S#Point =>
}
