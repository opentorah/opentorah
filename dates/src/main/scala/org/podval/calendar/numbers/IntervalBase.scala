package org.podval.calendar.numbers

abstract class IntervalBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Interval](digits)
{ this: S#Interval =>
  override def companion: NumberCompanion[S, S#Interval] = numberSystem.Interval

  private[this] def defaultLength: Int = numberSystem.defaultLength

  final def +(that: S#Interval): S#Interval =
    numberSystem.Interval.fromDigits(numberSystem.add(this.digits, that.digits))

  final def -(that: S#Interval): S#Interval =
    numberSystem.Interval.fromDigits(numberSystem.subtract(this.digits, that.digits))

  final def *(n: Int): S#Interval = fromDigits(digits map (n * _))

  final def /(n: Int, length: Int = defaultLength): S#Interval =
    numberSystem.Interval.fromRational(toRational / n, math.max(this.length, length))

//  final def %(n: Int, length: Int = defaultLength): S#Interval =
//    this - ((this / (n, length)) * n)

  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int = defaultLength): S#Interval = {
    //    ((that.head, 1) +: that.numberSystem.zipWithRanges(that.tail))
    //      .foldRight(newInterval(false, Seq(0))) { case ((digit: Int, range: Int), acc: S#Interval) =>
    //        (acc + this*digit)/ (range, length)
    //      }
    numberSystem.Interval.fromRational(this.toRational*that.toRational, length)
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

  final override def toInterval: S#Interval = this
}
