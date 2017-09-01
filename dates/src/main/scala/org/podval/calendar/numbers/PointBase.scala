package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class PointBase[S <: NumberSystem[S]](raw: RawNumber)
  extends Number[S, S#Point](raw)
{ this: S#Point =>
  protected final override def newNumber(raw: RawNumber): S#Point = numberSystem.Point(raw)

  final def +(that: S#Interval): S#Point = newNumber(add(negate = false, that))

  final def -(that: S#Interval): S#Point = newNumber(add(negate = true, that))

  final def -(that: S#Point): S#Interval = numberSystem.Interval(add(negate = true, that))

  final def toInterval: S#Interval = this - numberSystem.Point()
}
