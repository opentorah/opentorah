package org.podval.calendar.times

import org.podval.calendar.numbers.{Digits, NumberSystemMember, PointCompanion, VectorCompanion}

// For stand-alone testing of the Times NumberSystem.
class SimpleTimes extends Times[SimpleTimes] {
  final override type Point = TimePointBase[SimpleTimes]
  final override type Vector = TimeVectorBase[SimpleTimes]
  trait SimpleTimeNumberSystemMember extends NumberSystemMember[SimpleTimes] {
    final override def numberSystem: SimpleTimes = SimpleTimes.this
  }
  final override def createPoint(digits: Seq[Int]): TimePointBase[SimpleTimes] =
    new Digits(digits) with TimePointBase[SimpleTimes] with SimpleTimeNumberSystemMember
  final override def createVector(digits: Seq[Int]): TimeVectorBase[SimpleTimes] =
    new Digits(digits) with TimeVectorBase[SimpleTimes] with SimpleTimeNumberSystemMember
  final object Vector extends VectorCompanion[SimpleTimes] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanion[SimpleTimes] with SimpleTimeNumberSystemMember
}


object SimpleTimes extends SimpleTimes
