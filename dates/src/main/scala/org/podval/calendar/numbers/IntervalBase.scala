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

  final def /(n: Int, length: Int): S#Interval =
    newInterval(numberSystem.fromRational(toRational / n, length))

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

  // TODO unfold
  final def digitsWithRangesForMultiplication: List[(Int, Int)] =
    (head, 1) +: numberSystem.zipWithRanges(tail)

  // TODO redo via Rational! LongitudeMean uses the current implementation and it seems to work...
  // TODO do the suggested/real length thing
  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int): S#Interval = {
    val z = newInterval(false, List(0))

    def step(elem: (Int, Int), acc: S#Interval): S#Interval = {
      val (digit, range) = elem
      (acc + this*digit)/ (range, length)
    }

    that.digitsWithRangesForMultiplication.foldRight(z)(step)
  }

  // TODO add division and % on Intervals from another NumberSystem
}
