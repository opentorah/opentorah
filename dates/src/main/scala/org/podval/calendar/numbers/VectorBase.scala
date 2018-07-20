package org.podval.calendar.numbers

trait VectorBase[S <: Numbers[S]] extends Number[S, S#Vector]
{ this: S#Vector =>
  final def +(that: S#Vector): S#Vector = numbers.Vector.fromDigits(add(that))

  final def -(that: S#Vector): S#Vector = numbers.Vector.fromDigits(subtract(that))

  final def *(n: Int): S#Vector =
    numbers.Vector.fromDigits(digits map (n * _))

  final def /(n: Int): S#Vector = /(n, defaultLength)

  final def /(n: Int, length: Int): S#Vector =
    numbers.Vector.fromRational(toRational / BigRational(n), math.max(this.length, length))

  final def %(n: Int): S#Vector = %(n, defaultLength)

  final def %(n: Int, length: Int): S#Vector =
    this - ((this / (n, length)) * n)

  final def /(that: S#Vector): Int = (this.toRational / that.toRational).whole

  final def *(that: BigRational): S#Vector = *(that, defaultLength)

  final def *(that: BigRational, length: Int): S#Vector =
    numbers.Vector.fromRational(this.toRational*that, length)

  final def *[T <: Numbers[T]](that: T#Vector): S#Vector = *(that, defaultLength)

  final def *[T <: Numbers[T]](that: T#Vector, length: Int): S#Vector =
    *(that.toRational, length)

  final def %[T <: Numbers[T]](that: S#Vector, length: Int = defaultLength): S#Vector =
    this - (that * (this / that))

  private[this] def defaultLength: Int = numbers.defaultLength

  final override def companion: NumberCompanion[S, S#Vector] = numbers.Vector
  final override def toVector: S#Vector = this
  final override def toPoint: S#Point = numbers.Point() + this
}
