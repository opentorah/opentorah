package org.opentorah.calendar.numbers

/** Vector from the number system `S`.
  *
  * @tparam S  type of the number system
  */
trait VectorNumber[S <: Numbers[S]] extends Number[S, S#Vector] { this: S#Vector =>

  /** Returns Vector resulting from adding specified Vector to this one. */
  final def +(that: S#Vector): S#Vector = fromDigits(add(that))

  /** Returns Point resulting from adding specified Point to this Vector. */
  final def +(that: S#Point): S#Point = numbers.Point.fromDigits(add(that))

  /** Returns this Vector multiplied by the specified Int. */
  final def *(n: Int): S#Vector = fromDigits(digits map (_ * n))

  /** Returns this Vector divided by the specified Int with up to length digits after the point. */
  final def /(n: Int, length: Int): S#Vector = this*(BigRational(n).invert, length)

  /** Returns this Vector multiplied by the specified [[BigRational]] with up to length digits after the point. */
  final def *(that: BigRational, length: Int): S#Vector =
    companion.fromRational(this.toRational*that, math.max(this.length, length))

  /** Returns canonical representation of this Vector;
    * Vectors are not canonicalized by default even in the periodic number systems. */
  final def canonical: S#Vector = numbers.Vector.canonical(digits)
}
