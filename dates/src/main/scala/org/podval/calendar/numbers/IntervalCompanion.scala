package org.podval.calendar.numbers

abstract class IntervalCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Interval] {
  final override def apply(negative: Boolean, digits: Int*): S#Interval = {
    val (resultNegative: Boolean, resultDigits: Seq[Int]) = normalize(negative, digits)
    numberSystem.createInterval(resultNegative, resultDigits)
  }
}
