package org.opentorah.numbers

object Math {
  import Zeroable.ZeroableOps

  final def findZero[V : Zeroable](numbers: Numbers)
    (
      f: numbers.Point => V,
      left: numbers.Point,
      right: numbers.Point,
      length: Int
    ): numbers.Point =
    findZero0[V](numbers)(f, left, f(left), right, f(right), length)

  @scala.annotation.tailrec
  private final def findZero0[V : Zeroable](numbers: Numbers)
  (
    f: numbers.Point => V,
    left : numbers.Point, leftValue : V,
    right: numbers.Point, rightValue: V,
    length: Int
  ): numbers.Point = {
    val leftSignum : Int = leftValue.signum
    val rightSignum: Int = rightValue.signum
    println(s"$left $leftValue $leftSignum $right $rightValue $rightSignum $length")
    if (leftSignum == 0) left else if (rightSignum == 0) right else {
      require(leftSignum != rightSignum)
      val halfDistance: numbers.Vector = (right - left) / (2, length+1)
      if (halfDistance.roundTo(length).isZero) left else {
        val middle = left + halfDistance
        val middleValue: V = f(middle)
        val middleSignum: Int = middleValue.signum
        if (middleSignum == 0) middle else {
          if (middleSignum == leftSignum)
            findZero0[V](numbers)(f, middle, middleValue, right, rightValue, length)
          else
            findZero0[V](numbers)(f, left, leftValue, middle, middleValue, length)
        }
      }
    }
  }
}
