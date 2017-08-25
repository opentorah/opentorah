package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class IntervalBase[S <: NumberSystem[S]](raw: RawNumber)
  extends Number[S, S#Interval](raw)
{ this: S#Interval =>
  protected final override def newNumber(raw: RawNumber): S#Interval =  newInterval(raw)

  final def +(that: S#Interval): S#Interval = newInterval(add(negate = false, that))

  final def -(that: S#Interval): S#Interval = newInterval(add(negate = true, that))

  final def +(that: S#Point): S#Point = newPoint(add(negate = false, that))

  // TODO is this meaningful?
  final def -(that: S#Point): S#Point = newPoint(add(negate = true, that))

  final def *(n: Int): S#Interval = newInterval(negative, digits map (n * _))

  // TODO use maxLength instead of suggestedLength with 0 as default...
  // TODO redo via Rational!
  final def /(n: Int, suggestedLength: Int = 0): S#Interval = {
    def divide(digit: Int, range: Int, carry: Int): (Int, Int, Int) = {
      val value: Int = digit + carry*range
      (value, value / n, value % n)
    }

    def step(acc: (List[Int], Int), elem: (Int, Int)): (List[Int], Int) = {
      val (digit: Int, range: Int) = elem
      val (result: List[Int], carry: Int) = acc
      val (value: Int, quotient: Int, reminder: Int) = divide(digit, range, carry)
      (result :+ quotient, reminder)
    }

    def lastStep(digit: Int, range: Int, carry: Int): Int = {
      val (value: Int, quotient: Int, reminder: Int) = divide(digit, range, carry)
      val roundUp: Boolean =
        ((n % 2 == 0) && (reminder >= n / 2)) || ((n % 2 == 1) && (reminder > n / 2))
      if (roundUp) quotient+1 else quotient
    }

    val realLength: Int = Math.max(length, suggestedLength)

    val (newDigits, lastCarry) =
      ((digits.head, 0) +:  numberSystem.zipWithRanges(tail.padTo(realLength-1, 0))) // centralize padTo()
      .foldLeft(List.empty[Int], 0)(step)
    val lastDigit =
      lastStep(0, numberSystem.range(realLength-1), lastCarry)
    newInterval(negative, newDigits :+ lastDigit)
  }

  // TODO do the suggested/real length thing
  // TODO redo via Rational!
  final def %(n: Int, length: Int): S#Interval = this - ((this / (n, length)) * n)

  // TODO redo via Rational!
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

  // TODO redo via Rational!
  final def %(that: S#Interval): S#Interval = this - (that * (this / that))

  // TODO add multiplication (and division, and %) on Intervals from another NumberSystem!

  // TODO unfold
  final def digitsWithRangesForMultiplication: List[(Int, Int)] =
    (head, 1) +: numberSystem.zipWithRanges(tail)

  // TODO redo via Rational!
  // TODO do the suggested/real length thing
  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int): S#Interval = {
    val z = newInterval(false, List(0))

    def step(elem: (Int, Int), acc: S#Interval): S#Interval = {
      val (digit, range) = elem
      (acc + this*digit)/ (range, length)
    }

    that.digitsWithRangesForMultiplication.foldRight(z)(step)
  }
}
