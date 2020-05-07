package org.opentorah.numbers

/** Point from the number system `S`.
  *
  * @tparam S  type of the number system
  */
trait PointNumber[S <: Numbers[S]] extends Number[S, S#Point] { this: S#Point =>

  /** Returns Point resulting from adding specified Vector to this one. */
  final def +(that: S#Vector): S#Point = fromDigits(add(that))

  /** Returns Point resulting subtracting specified Vector to this one. */
  final def -(that: S#Vector): S#Point = fromDigits(subtract(that))
}
