package org.podval.calendar.numbers

abstract class IntervalCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Interval] {
  final override def apply(digits: Int*): S#Interval =
    numberSystem.createInterval(normalize(digits))
}
