package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class IntervalBase[S <: NumberSystem[S]](raw: RawNumber)
  extends Number[S, S#Interval](raw)
{ this: S#Interval =>
  protected final override def newN(raw: RawNumber): S#Interval =  newInterval(raw)

  final def +(that: S#Interval): S#Interval = newInterval(add(negate = false, that))

  final def -(that: S#Interval): S#Interval = newInterval(add(negate = true, that))

  final def +(that: S#Point): S#Point = newPoint(add(negate = false, that))

  // TODO -(Point)?

  final def *(n: Int): S#Interval = newInterval(negative, digits map (n * _))

  final def /(n: Int): S#Interval = {
    def step(acc: (List[Int], Int), elem: (Int, Int)) = {
      val (digit, range) = elem
      val (result, carry) = acc
      val value = digit + carry*range
      val (quotient, reminder) = (value / n, value % n)

      (result :+ quotient, reminder)
    }

    def lastStep(last: Int, lastCarry: Int, lastRange: Int): Int = {
      val value = last + lastCarry*lastRange
      val (quotient, reminder) = (value / n, value % n)

      val roundUp = ((n % 2 == 0) && (reminder >= n / 2)) || ((n % 2 == 1) && (reminder > n / 2))

      if (roundUp) quotient+1 else quotient
    }

    val digits = this.digits.padTo(numberSystem.maxLength + 1, 0)
    val (newDigits, lastCarry) = (digits.init zip (0 :: numberSystem.ranges.init)).
      foldLeft(List.empty[Int], 0)(step)
    val lastDigit = lastStep(digits.last, lastCarry, numberSystem.ranges.last)

    newInterval(negative, newDigits :+ lastDigit)
  }

  final def %(n: Int): S#Interval = this - ((this / n) * n)

  final def /(that: S#Interval): Int = {
    // TODO deal with negativity
    // TODO faster?
    var result = 0
    var done = false
    var subtractee = this

    do {
      subtractee = subtractee - that
      done = subtractee.negative
      if (!done) result += 1
    } while (!done)

    result
  }

  final def %(that: S#Interval): S#Interval = this - (that * (this / that))

  // TODO add multiplication (and division, and %) on Intervals from another NumberSystem!

  final def digitsWithRangesForMultiplication: List[(Int, Int)] =
    digits zip (1 :: numberSystem.ranges)

  final def *[T <: NumberSystem[T]](that: T#Interval): S#Interval = {
    val z = newInterval(false, List(0))

    def step(elem: (Int, Int), acc: S#Interval): S#Interval = {
      val (digit, range) = elem
      (acc + this*digit)/range
    }

    that.digitsWithRangesForMultiplication.foldRight(z)(step)
  }
}
