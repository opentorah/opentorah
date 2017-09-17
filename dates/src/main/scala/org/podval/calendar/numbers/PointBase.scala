package org.podval.calendar.numbers

abstract class PointBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Point](digits)
{ this: S#Point =>
  override def companion: NumberCompanion[S, S#Point] = numberSystem.Point

  final def +(that: S#Interval): S#Point =
    numberSystem.Point.fromDigits(numberSystem.add(this.digits, that.digits))

  final def -(that: S#Interval): S#Point =
    numberSystem.Point.fromDigits(numberSystem.subtract(this.digits, that.digits))

  final def -(that: S#Point): S#Interval =
    numberSystem.Interval.fromDigits(numberSystem.subtract(this.digits, that.digits))

  final override def toInterval: S#Interval = this - numberSystem.Point()
}
