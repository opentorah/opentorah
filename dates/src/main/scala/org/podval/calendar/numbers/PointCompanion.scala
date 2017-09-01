package org.podval.calendar.numbers

import NumberSystem.RawNumber

abstract class PointCompanion[S <: NumberSystem[S]] extends NumberCompanion[S, S#Point] {
  final override def newNumber(raw: RawNumber): S#Point =
    numberSystem.createPoint(numberSystem.normalize(raw))
}
