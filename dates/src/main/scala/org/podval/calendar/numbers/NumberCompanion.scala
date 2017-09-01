package org.podval.calendar.numbers

import NumberSystem.RawNumber

trait NumberCompanion[S <: NumberSystem[S], N <: Number[S, N]]
  extends NumberSystemMember[S]
{
  def newNumber(raw: RawNumber): N

  final def apply(raw: RawNumber): N = newNumber(raw)

  final def apply(negative: Boolean, digits: Int*): N =
    apply((negative, (if (digits.nonEmpty) digits else Seq(0)).toList))

  final def apply(digits: Int*): N = apply(negative = false, digits: _*)
}
