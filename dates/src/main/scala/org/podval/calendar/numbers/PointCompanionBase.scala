package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class PointCompanionBase[S <: NumberSystem[S]] extends NumberSystemMember[S] {
  final def apply(raw: RawNumber): S#Point = numberSystem.newPoint(raw)

  final def apply(negative: Boolean, digits: Int*): S#Point =
    apply((negative, (if (digits.nonEmpty) digits else Seq(0)).toList))

  final def apply(digits: Int*): S#Point = apply(negative = false, digits: _*)
}
