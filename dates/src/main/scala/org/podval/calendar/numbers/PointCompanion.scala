package org.podval.calendar.numbers

abstract class PointCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Point] {
  final override def apply(negative: Boolean, digits: Int*): S#Point = {
    val (resultNegative: Boolean, resultDigits: Seq[Int]) = normalize(negative, digits)
    numberSystem.createPoint(resultNegative, resultDigits)
  }
}
