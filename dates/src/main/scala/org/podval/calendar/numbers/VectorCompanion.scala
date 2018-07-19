package org.podval.calendar.numbers

abstract class VectorCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Vector] {
  final override def apply(digits: Int*): S#Vector = numberSystem.createVector(digits)
}
