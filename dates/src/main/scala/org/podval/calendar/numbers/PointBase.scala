package org.podval.calendar.numbers

trait PointBase[S <: Numbers[S]] extends Number[S, S#Point]
{ this: S#Point =>
  override def companion: NumberCompanion[S, S#Point] = numberSystem.Point

  final def +(that: S#Vector): S#Point = numberSystem.Point.fromDigits(add(that))

  final def -(that: S#Vector): S#Point = numberSystem.Point.fromDigits(subtract(that))

  final def -(that: S#Point): S#Vector = numberSystem.Vector.fromDigits(subtract(that))

  final override def toVector: S#Vector = this - numberSystem.Point()
  final override def toPoint: S#Point = this
}
