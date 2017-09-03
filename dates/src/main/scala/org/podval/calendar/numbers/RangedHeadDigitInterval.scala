package org.podval.calendar.numbers

abstract class RangedHeadDigitInterval[S <: RangedHeadDigitNumberSystem[S]]
  (negative: Boolean, digits: Seq[Int])
  extends IntervalBase[S](negative, digits) with RangedHeadDigitNumber[S, S#Interval]
{ this: S#Interval =>
}
