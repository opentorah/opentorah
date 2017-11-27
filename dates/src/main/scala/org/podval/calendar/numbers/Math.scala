package org.podval.calendar.numbers

object Math {

  // TODO two separate lengths (precisions)?
  // TODO unify precisions/lengths and digit positions.
  final def findZero[S <: NumberSystem[S], T <: NumberSystem[T]]
    (f: S#Point => T#Point, left: S#Point, right: S#Point, length: Int): S#Point =
  {
    val leftValue : T#Point = f(left)
    val rightValue: T#Point = f (right)
    findZero(f, left, leftValue, leftValue.signum, right, rightValue, rightValue.signum, length)
  }

  private final def findZero[S <: NumberSystem[S], T <: NumberSystem[T]](
    f: S#Point => T#Point,
    left : S#Point, leftValue : T#Point, leftSignum : Int,
    right: S#Point, rightValue: T#Point, rightSignum: Int,
    length: Int
  ): S#Point = {
    println(s"$left $leftValue $leftSignum $right $rightValue $rightSignum $length")
    if (leftSignum == 0) left else if (rightSignum == 0) right else {
      require(leftSignum != rightSignum)
      val halfDistance: S#Interval = (right - left) / (2, length+1)
      if (halfDistance.roundTo(length).isZero) left else {
        val middle = left + halfDistance
        val middleValue: T#Point = f(middle)
        val middleSignum: Int = middleValue.signum
        if (middleSignum == 0) middle else {
          if (middleSignum == leftSignum)
            findZero(f, middle, middleValue, middleSignum, right, rightValue, rightSignum, length)
          else
            findZero(f, left, leftValue, leftSignum, middle, middleValue, middleSignum, length)
        }
      }
    }
  }
}
