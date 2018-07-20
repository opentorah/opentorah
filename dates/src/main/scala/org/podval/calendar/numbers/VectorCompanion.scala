package org.podval.calendar.numbers

abstract class VectorCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Vector] {
  final override def apply(digits: Int*): S#Vector = numbers.createVector(digits)
}
