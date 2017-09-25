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

  // TODO if length has a default (defaultLength), this method gets confused with the *(S#Interval)...
  final def *(r: BigRational, length: Int): S#Interval =
    numberSystem.Interval.fromRational(this.toRational*r, length)

  final def *[T <: NumberSystem[T]](that: T#Interval, length: Int = defaultLength): S#Interval =
    numberSystem.Interval.fromRational(this.toRational*that.toRational, length)

  final def /(that: S#Interval): Int = (this.toRational / that.toRational).wholeAndFraction._1

  final def %[T <: NumberSystem[T]](that: S#Interval, length: Int = defaultLength): S#Interval = {
    // TODO: law:
    this - (that * (this / that))
    // TODO why isn't this equivalent?
//    val (whole: Int, fraction: BigRational) = (this.toRational / that.toRational).wholeAndFraction
//    newInterval(fromRational(fraction, length))
  }

  final override def toInterval: S#Interval = this
  final override def toPoint   : S#Point    = numberSystem.Point() + this
}
