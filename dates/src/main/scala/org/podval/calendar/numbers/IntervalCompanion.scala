package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class IntervalCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Interval] {
  final override def newNumber(raw: RawNumber): S#Interval = numberSystem.newInterval(raw)
}
