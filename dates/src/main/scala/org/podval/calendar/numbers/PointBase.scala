package org.podval.calendar.numbers

abstract class PointBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Point](digits)
{ this: S#Point =>
  override def companion: NumberCompanion[S, S#Point] = numberSystem.Point

  final def +(that: S#Interval): S#Point = numberSystem.Point.fromDigits(add(that))

  final def -(that: S#Interval): S#Point = numberSystem.Point.fromDigits(subtract(that))

  final def -(that: S#Point): S#Interval = numberSystem.Interval.fromDigits(subtract(that))

  final override def toInterval: S#Interval = this - numberSystem.Point()
  final override def toPoint   : S#Point    = this
}
