package org.podval.calendar.numbers

trait VectorBase[S <: Numbers[S]] extends Number[S, S#Vector]
{ this: S#Vector =>
  final def +(that: S#Vector): S#Vector = fromDigits(add(that))

  final def +(that: S#Point): S#Point = numbers.Point.fromDigits(add(that))

  final def *(n: Int): S#Vector = fromDigits(digits map (_ * n))

  final def /(n: Int, length: Int): S#Vector = this*(BigRational(n).invert, length)

  final def *(that: BigRational, length: Int): S#Vector =
    companion.fromRational(this.toRational*that, math.max(this.length, length))
}
