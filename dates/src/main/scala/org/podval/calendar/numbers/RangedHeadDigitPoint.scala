package org.podval.calendar.numbers

abstract class RangedHeadDigitPoint[S <: RangedHeadDigitNumberSystem[S]]
  (negative: Boolean, digits: Seq[Int])
  extends PointBase[S](negative, digits) with RangedHeadDigitNumber[S, S#Point]
{ this: S#Point =>
}
