package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class PointCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Point] {
  final override def apply(raw: RawNumber): S#Point = numberSystem.createPoint(normalize(raw))
}
