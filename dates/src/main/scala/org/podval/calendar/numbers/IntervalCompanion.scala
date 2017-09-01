package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class IntervalCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Interval] {
  final override def apply(raw: RawNumber): S#Interval = numberSystem.createInterval(normalize(raw))
}
