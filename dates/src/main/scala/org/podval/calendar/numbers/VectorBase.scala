package org.podval.calendar.numbers

abstract class VectorBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Vector](digits)
{ this: S#Vector =>
  final def +(that: S#Vector): S#Vector = numberSystem.Vector.fromDigits(add(that))

  final def -(that: S#Vector): S#Vector = numberSystem.Vector.fromDigits(subtract(that))

  final def *(n: Int): S#Vector =
    numberSystem.Vector.fromDigits(digits map (n * _))

  final def /(n: Int): S#Vector = /(n, defaultLength)

  final def /(n: Int, length: Int): S#Vector =
    numberSystem.Vector.fromRational(toRational / n, math.max(this.length, length))

  final def %(n: Int): S#Vector = %(n, defaultLength)

  final def %(n: Int, length: Int): S#Vector =
    this - ((this / (n, length)) * n)

  final def /(that: S#Vector): Int = (this.toRational / that.toRational).whole

  final def *(that: BigRational): S#Vector = *(that, defaultLength)

  final def *(that: BigRational, length: Int): S#Vector =
    numberSystem.Vector.fromRational(this.toRational*that, length)

  final def *[T <: NumberSystem[T]](that: T#Vector): S#Vector = *(that, defaultLength)

  final def *[T <: NumberSystem[T]](that: T#Vector, length: Int): S#Vector =
    *(that.toRational, length)

  final def %[T <: NumberSystem[T]](that: S#Vector, length: Int = defaultLength): S#Vector =
    this - (that * (this / that))

  private[this] def defaultLength: Int = numberSystem.defaultLength

  final override def companion: NumberCompanion[S, S#Vector] = numberSystem.Vector
  final override def toVector: S#Vector = this
  final override def toPoint: S#Point = numberSystem.Point() + this
}
