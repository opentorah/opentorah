package org.podval.calendar.numbers

trait PointBase[S <: Numbers[S]] extends Number[S, S#Point]
{ this: S#Point =>
  final def +(that: S#Vector): S#Point = fromDigits(add(that))

  final def -(that: S#Vector): S#Point = fromDigits(subtract(that))
}
