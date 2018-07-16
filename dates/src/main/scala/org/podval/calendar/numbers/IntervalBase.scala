package org.podval.calendar.numbers

abstract class IntervalBase[S <: NumberSystem[S]](digits: Seq[Int])
  extends Number[S, S#Interval](digits)
{ this: S#Interval =>
  final def +(that: S#Interval): S#Interval = numberSystem.Interval.fromDigits(add(that))

  final def -(that: S#Interval): S#Interval = numberSystem.Interval.fromDigits(subtract(that))

  final def *(n: Int): S#Interval =
    numberSystem.Interval.fromDigits(digits map (n * _))

  final def /(n: Int): S#Interval = /(n, defaultLength)

  // TODO introduce implicit parameter of type Precision?

  final def /(n: Int, length: Int): S#Interval =
    numberSystem.Interval.fromRational(toRational / n, math.max(this.length, length))

  final def %(n: Int): S#Interval = %(n, defaultLength)

  final def %(n: Int, length: Int): S#Interval =
    this - ((this / (n, length)) * n)

  final def /(that: S#Interval): Int = // TODO return BigRational instead?
    (this.toRational / that.toRational).wholeAndFraction._1

  final def *(that: BigRational): S#Interval = *(that, defaultLength)

  final def *(that: BigRational, length: Int): S#Interval =
    numberSystem.Interval.fromRational(this.toRational*that, length)

  // TODO def %(BigRational)?

  final def *[T <: NumberSystem[T]](that: T#Interval): S#Interval = *(that, defaultLength)

  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int): S#Interval =
    *(that.toRational, length)

  final def %[T <: NumberSystem[T]](that: S#Interval, length: Int = defaultLength): S#Interval =
    this - (that * (this / that))

  private[this] def defaultLength: Int = numberSystem.defaultLength

  final override def companion: NumberCompanion[S, S#Interval] = numberSystem.Interval
  final override def toInterval: S#Interval = this
  final override def toPoint   : S#Point    = numberSystem.Point() + this
}
