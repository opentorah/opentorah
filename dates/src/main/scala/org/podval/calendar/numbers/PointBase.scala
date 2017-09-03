package org.podval.calendar.numbers

abstract class PointBase[S <: NumberSystem[S]](negative: Boolean, digits: Seq[Int])
  extends Number[S, S#Point](negative, digits)
{ this: S#Point =>
  protected final override def newNumber(negative: Boolean, digits: Seq[Int]): S#Point =
    numberSystem.Point((negative, digits))

  final def +(that: S#Interval): S#Point = numberSystem.Point(add(negate = false, that))

  final def -(that: S#Interval): S#Point = numberSystem.Point(add(negate = true, that))

  final def -(that: S#Point): S#Interval = {
    val (negative: Boolean, digits: Seq[Int]) = add(negate = true, that)
    numberSystem.Interval((negative, digits))
  }

  final def toInterval: S#Interval = this - numberSystem.Point()
}
