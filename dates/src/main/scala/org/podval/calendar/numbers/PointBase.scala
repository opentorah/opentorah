package org.podval.calendar.numbers

trait PointBase[S <: Numbers[S]] extends Number[S, S#Point]
{ this: S#Point =>
  override def companion: NumberCompanion[S, S#Point] = numbers.Point

  final def +(that: S#Vector): S#Point = numbers.Point.fromDigits(add(that))

  final def -(that: S#Vector): S#Point = numbers.Point.fromDigits(subtract(that))

  final def -(that: S#Point): S#Vector = numbers.Vector.fromDigits(subtract(that))

  final override def toVector: S#Vector = this - numbers.Point()
  final override def toPoint: S#Point = this
}
