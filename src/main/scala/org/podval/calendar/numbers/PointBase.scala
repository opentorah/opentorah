package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class PointBase[T <: NumberSystem[T]](raw: RawNumber) extends Number[T, T#Point](raw) { this: T#Point =>
  protected final override def newN(raw: RawNumber): T#Point = newPoint(raw)

  final def +(that: T#Interval): T#Point = newPoint(plusMinus(operationNegation = false, that))

  final def -(that: T#Interval): T#Point = newPoint(plusMinus(operationNegation = true, that))

  final def -(that: T#Point): T#Interval = newInterval(plusMinus(operationNegation = true, that))
}
