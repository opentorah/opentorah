package org.podval.calendar.numbers

abstract class IntervalBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Interval](digits)
{ this: S#Interval =>
  final def +(that: S#Interval): S#Interval =
    numberSystem.Interval.fromDigits(numberSystem.add(this, that))

  final def -(that: S#Interval): S#Interval =
    numberSystem.Interval.fromDigits(numberSystem.subtract(this, that))

  final def *(n: Int): S#Interval =
    numberSystem.Interval.fromDigits(digits map (n * _))

  final def /(n: Int, length: Int = defaultLength): S#Interval =
    numberSystem.Interval.fromRational(toRational / n, math.max(this.length, length))

  final def %(n: Int, length: Int = defaultLength): S#Interval =
    this - ((this / (n, length)) * n)

  final def /(that: S#Interval): Int =
    (this.toRational / that.toRational).wholeAndFraction._1

  final def multRational(that: BigRational, length: Int = defaultLength): S#Interval =
    numberSystem.Interval.fromRational(this.toRational*that, length)

  final def multNumber[T <: NumberSystem[T]](that: T#Interval, length: Int = defaultLength): S#Interval =
    multRational(that.toRational, length)

  final def remNumber[T <: NumberSystem[T]](that: S#Interval, length: Int = defaultLength): S#Interval =
    this - (that * (this / that))

  private[this] def defaultLength: Int = numberSystem.defaultLength

  final override def companion: NumberCompanion[S, S#Interval] = numberSystem.Interval
  final override def toInterval: S#Interval = this
  final override def toPoint   : S#Point    = numberSystem.Point() + this
}
