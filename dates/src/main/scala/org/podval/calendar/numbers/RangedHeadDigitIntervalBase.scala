package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO drop "Base"
abstract class RangedHeadDigitIntervalBase[S <: RangedHeadDigitNumberSystem[S]](raw: RawNumber)
  extends IntervalBase[S](raw) with RangedHeadDigitNumber[S, S#Interval]
{ this: S#Interval =>
}
