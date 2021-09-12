package org.opentorah.numbers

/**
  * Operations needed to find a zero crossing of a function.
  *
  * @tparam T  type of the number to convert to/from
  */
trait Zeroable[T]:
  def signum(value: T): Int

object Zeroable:
  extension[T: Zeroable](value: T)
    def signum: Int = summon[Zeroable[T]].signum(value)

//  import Zeroable.signum

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
    ): numbers.Point =
    val leftSignum : Int = leftValue.signum
    val rightSignum: Int = rightValue.signum
    println(s"$left $leftValue $leftSignum $right $rightValue $rightSignum $length")
    if leftSignum == 0 then left else if rightSignum == 0 then right else
      require(leftSignum != rightSignum)
      val halfDistance: numbers.Vector = (right - left) / (2, length+1)
      if halfDistance.roundTo(length).isZero then left else
        val middle = left + halfDistance
        val middleValue: V = f(middle)
        val middleSignum: Int = middleValue.signum
        if middleSignum == 0 then middle else
          if middleSignum == leftSignum then
            findZero0[V](numbers)(f, middle, middleValue, right, rightValue, length)
          else
            findZero0[V](numbers)(f, left, leftValue, middle, middleValue, length)