package org.podval.calendar.numbers

abstract class PointCompanion[S <: Numbers[S]] extends NumberCompanion[S, S#Point] {
  final override def apply(digits: Int*): S#Point = numberSystem.createPoint(digits)
}
