package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class PointCompanionBase[S <: NumberSystem[S]] extends NumberCompanion[S, S#Point] {
  final override def newNumber(raw: RawNumber): S#Point = numberSystem.newPoint(raw)
}
