package org.podval.calendar.time

import org.podval.calendar.numbers.{VectorCompanion, NumberSystemMember, PointCompanion}

// For stand-alone testing of the TimeNumberSystem.
class SimpleTimeNumberSystem extends TimeNumberSystem[SimpleTimeNumberSystem] {
  final override type Point = TimePointBase[SimpleTimeNumberSystem]
  final override type Vector = TimeVectorBase[SimpleTimeNumberSystem]
  trait SimpleTimeNumberSystemMember extends NumberSystemMember[SimpleTimeNumberSystem] {
    final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
  }
  final override def createPoint(digits: Seq[Int]): TimePointBase[SimpleTimeNumberSystem] =
    new TimePointBase[SimpleTimeNumberSystem](digits) with SimpleTimeNumberSystemMember
  final override def createVector(digits: Seq[Int]): TimeVectorBase[SimpleTimeNumberSystem] =
    new TimeVectorBase[SimpleTimeNumberSystem](digits) with SimpleTimeNumberSystemMember
  final object Vector extends VectorCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
