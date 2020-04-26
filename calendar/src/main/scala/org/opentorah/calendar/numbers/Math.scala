package org.opentorah.calendar.numbers

object Math {
  import Zeroable.ZeroableOps

  final def findZero[S <: Numbers[S], V : Zeroable]
    (f: S#Point => V, left: S#Point, right: S#Point, length: Int): S#Point =
    findZero(f, left, f(left), right, f(right), length)

  @scala.annotation.tailrec
  private final def findZero[S <: Numbers[S], V : Zeroable](
    f: S#Point => V,
    left : S#Point, leftValue : V,
    right: S#Point, rightValue: V,
    length: Int
  ): S#Point = {
    val leftSignum : Int = leftValue.signum
    val rightSignum: Int = rightValue.signum
    println(s"$left $leftValue $leftSignum $right $rightValue $rightSignum $length")
    if (leftSignum == 0) left else if (rightSignum == 0) right else {
      require(leftSignum != rightSignum)
      val halfDistance: S#Vector = (right - left) / (2, length+1)
      if (halfDistance.roundTo(length).isZero) left else {
        val middle = left + halfDistance
        val middleValue: V = f(middle)
        val middleSignum: Int = middleValue.signum
        if (middleSignum == 0) middle else {
          if (middleSignum == leftSignum)
            findZero(f, middle, middleValue, right, rightValue, length)
          else
            findZero(f, left, leftValue, middle, middleValue, length)
        }
      }
    }
  }
}
