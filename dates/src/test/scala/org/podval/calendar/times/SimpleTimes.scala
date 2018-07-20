package org.podval.calendar.times

import org.podval.calendar.numbers.{Digits, NumbersMember, PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  final override type Point = TimePointBase[SimpleTimes]
  final override type Vector = TimeVectorBase[SimpleTimes]
  trait SimpleTimesMember extends NumbersMember[SimpleTimes] {
    final override def numbers: SimpleTimes = SimpleTimes.this
  }
  final override def createPoint(digits: Seq[Int]): TimePointBase[SimpleTimes] =
    new Digits(digits) with TimePointBase[SimpleTimes] with SimpleTimesMember
  final override def createVector(digits: Seq[Int]): TimeVectorBase[SimpleTimes] =
    new Digits(digits) with TimeVectorBase[SimpleTimes] with SimpleTimesMember
  final object Vector extends VectorCompanion[SimpleTimes] with SimpleTimesMember
  final object Point extends PointCompanion[SimpleTimes] with SimpleTimesMember
}


object SimpleTimes extends SimpleTimes
