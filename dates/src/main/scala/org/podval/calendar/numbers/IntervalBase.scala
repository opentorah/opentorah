package org.podval.calendar.numbers

import org.podval.calendar.numbers.NumberSystem.RawNumber

abstract class IntervalBase[S <: NumberSystem[S]](raw: RawNumber)
  extends Number[S, S#Interval](raw)
{ this: S#Interval =>
  protected final override def newNumber(raw: RawNumber): S#Interval =  newInterval(raw)

  private[this] def fromRational(value: BigRational, length: Int): RawNumber =
    numberSystem.fromRational(value, length)

  private[this] def defaultLength: Int = numberSystem.defaultLength

  final def +(that: S#Interval): S#Interval = newInterval(add(negate = false, that))

  final def -(that: S#Interval): S#Interval = newInterval(add(negate = true, that))

  final def *(n: Int): S#Interval = newInterval(negative, digits map (n * _))

  final def /(n: Int, length: Int = defaultLength): S#Interval =
    newInterval(fromRational(toRational / n, math.max(this.length, length)))

//  final def %(n: Int, length: Int = defaultLength): S#Interval =
//    this - ((this / (n, length)) * n)

  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int = defaultLength): S#Interval = {
    //    ((that.head, 1) +: that.numberSystem.zipWithRanges(that.tail))
    //      .foldRight(newInterval(false, List(0))) { case ((digit: Int, range: Int), acc: S#Interval) =>
    //        (acc + this*digit)/ (range, length)
    //      }
    newInterval(fromRational(this.toRational*that.toRational, length))
  }

  final def /(that: S#Interval): Int = {
    val (whole: Int, fraction: BigRational) = (this.toRational / that.toRational).wholeAndFraction
    whole
  }

  final def %[T <: NumberSystem[T]](that: S#Interval, length: Int = defaultLength): S#Interval = {
    // TODO: law:
    this - (that * (this / that))
    // TODO why isn't this equivalent?
//    val (whole: Int, fraction: BigRational) = (this.toRational / that.toRational).wholeAndFraction
//    newInterval(fromRational(fraction, length))
  }
}
