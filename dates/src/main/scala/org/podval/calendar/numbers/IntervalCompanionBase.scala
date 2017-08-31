package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class IntervalCompanionBase[S <: NumberSystem[S]] extends NumberSystemMember[S] {
  final def apply(raw: RawNumber): S#Interval = numberSystem.newInterval(raw)

  final def apply(negative: Boolean, digits: Int*): S#Interval =
    apply((negative, (if (digits.nonEmpty) digits else Seq(0)).toList))

  final def apply(digits: Int*): S#Interval = apply(negative = false, digits: _*)

  final def apply(): S#Interval = apply(false, 0)
}
