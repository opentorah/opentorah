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


  final class Exactify(val numbers: Numbers.Periodic)(
    small: numbers.Vector,
    mult: Int,
    round: Int,
    big: numbers.Vector
  ):

    def findFit: (numbers.Vector.Interval, Int) = findFit0(round)

    private def findFit0(length: Int): (numbers.Vector.Interval, Int) =
      val result = find(/*squeeze(*/enclose(length)/*, length)*/, length)
      require(fits(result._1))
      result

    @scala.annotation.tailrec
    private def find(interval: numbers.Vector.Interval, length: Int): (numbers.Vector.Interval, Int) =
      if fits(interval) then (interval, length) else find(squeeze(interval, length+1), length+1)

    private def enclose(length: Int): numbers.Vector.Interval =
      val step: numbers.Vector = stepForLength(length)

      var from: numbers.Vector = small
      var to: numbers.Vector = small

      while (calculate(from) > big) && from.isPositive do from = from-step
      while  calculate(to  ) < big                     do to   = to  +step

      val result = numbers.Vector.Interval(from, to)

      require(encloses(result))
      result

    private def squeeze(interval: numbers.Vector.Interval, length: Int): numbers.Vector.Interval =
      require(encloses(interval))

      val step: numbers.Vector = stepForLength(length)

      var from: numbers.Vector = interval.from
      var to: numbers.Vector = interval.to

      while (from <= to) && (calculate(from) < big) && (calculate(from+step) <= big)                  do from = from+step
      while (from <= to) && (calculate(to  ) > big) && (calculate(to  -step) >= big) && to.isPositive do to   = to  -step

      val result = numbers.Vector.Interval(from, to)

      require(encloses(result))
      result

    @scala.annotation.tailrec
    def expand(interval: numbers.Vector.Interval, length: Int, toLength: Int): numbers.Vector.Interval =
      if length == toLength then interval else expand(expand1(interval, length+1), length+1, toLength)

    private def expand1(interval: numbers.Vector.Interval, length: Int): numbers.Vector.Interval =
      require(fits(interval))

      val step: numbers.Vector = stepForLength(length)

      var from: numbers.Vector = interval.from
      var to: numbers.Vector = interval.to

      while calculate(from-step) == big do from = from-step
      while calculate(to  +step) == big do to   = to  +step

      val result = numbers.Vector.Interval(from, to)

      require(fits(result))
      result

    private def calculate(arg: numbers.Vector): numbers.Vector = (arg*mult).canonical.roundTo(round)

    private def stepForLength(length: Int): numbers.Vector = numbers.Vector(0).set(length, 1)

    private def encloses(interval: numbers.Vector.Interval): Boolean = (calculate(interval.from) <= big) && (calculate(interval.to) >= big)

    private def fits(interval: numbers.Vector.Interval): Boolean =
      def fits(arg: numbers.Vector): Boolean = calculate(arg) == big
      fits(interval.from) && fits(interval.to)
