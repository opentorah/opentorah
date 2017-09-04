package org.podval.calendar.numbers

abstract class PointBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Point](digits)
{ this: S#Point =>
  protected final override def fromDigits(digits: Seq[Int]): S#Point =
    numberSystem.Point.fromDigits(digits)

  final def +(that: S#Interval): S#Point =
    numberSystem.Point.fromDigits(add(negate = false, that))

  final def -(that: S#Interval): S#Point =
    numberSystem.Point.fromDigits(add(negate = true, that))

  final def -(that: S#Point): S#Interval =
    numberSystem.Interval.fromDigits(add(negate = true, that))

  final def toInterval: S#Interval = this - numberSystem.Point()
}
