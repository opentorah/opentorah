package org.opentorah.numbers

object Math:

  final def findZero(numbers: Numbers, vectorNumbers: Numbers)
    (
      f: numbers.Point => vectorNumbers.Vector,
      left: numbers.Point,
      right: numbers.Point,
      length: Int
    ): numbers.Point =
    findZero0(numbers, vectorNumbers)(f, left, f(left), right, f(right), length)

  @scala.annotation.tailrec
  private final def findZero0(numbers: Numbers, vectorNumbers: Numbers)
    (
      f: numbers.Point => vectorNumbers.Vector,
      left : numbers.Point, leftValue : vectorNumbers.Vector,
      right: numbers.Point, rightValue: vectorNumbers.Vector,
      length: Int
    ): numbers.Point =
      val leftSignum : Int = leftValue.signum
      val rightSignum: Int = rightValue.signum
      println(s"$left $leftValue $leftSignum $right $rightValue $rightSignum $length")
      if leftSignum == 0 then left else if rightSignum == 0 then right else
        require(leftSignum != rightSignum)
        val halfDistance: numbers.Vector = (right - left) / (2, length+1)
        if halfDistance.roundTo(length).isZero then left else
          val middle = left + halfDistance
          val middleValue: vectorNumbers.Vector = f(middle)
          val middleSignum: Int = middleValue.signum
          if middleSignum == 0 then middle else
            if middleSignum == leftSignum then
              findZero0(numbers, vectorNumbers)(f, middle, middleValue, right, rightValue, length)
            else
              findZero0(numbers, vectorNumbers)(f, left, leftValue, middle, middleValue, length)
