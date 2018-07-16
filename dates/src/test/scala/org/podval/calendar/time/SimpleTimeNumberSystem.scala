package org.podval.calendar.time

import org.podval.calendar.numbers.{IntervalCompanion, NumberSystemMember, PointCompanion}

// For stand-alone testing of the TimeNumberSystem.
class SimpleTimeNumberSystem extends TimeNumberSystem[SimpleTimeNumberSystem] {
  final override type Point = TimePointBase[SimpleTimeNumberSystem]
  final override type Interval = TimeIntervalBase[SimpleTimeNumberSystem]
  trait SimpleTimeNumberSystemMember extends NumberSystemMember[SimpleTimeNumberSystem] {
    final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
  }
  final override def createPoint(digits: Seq[Int]): TimePointBase[SimpleTimeNumberSystem] =
    new TimePointBase[SimpleTimeNumberSystem](digits) with SimpleTimeNumberSystemMember
  final override def createInterval(digits: Seq[Int]): TimeIntervalBase[SimpleTimeNumberSystem] =
    new TimeIntervalBase[SimpleTimeNumberSystem](digits) with SimpleTimeNumberSystemMember
  final object Interval extends IntervalCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
