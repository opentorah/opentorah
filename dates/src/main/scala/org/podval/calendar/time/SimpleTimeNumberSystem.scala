package org.podval.calendar.time

import org.podval.calendar.numbers.{IntervalCompanion, NumberSystemMember, PointCompanion}

// This exists so that TimeNumberSystem could be tested stand-alone.
// TODO "lift" tests to Calendar and eliminate?
class SimpleTimeNumberSystem extends TimeNumberSystem[SimpleTimeNumberSystem] {
  final override type Point = TimePointBase[SimpleTimeNumberSystem]
  final override type Interval = TimeIntervalBase[SimpleTimeNumberSystem]
  trait SimpleTimeNumberSystemMember extends NumberSystemMember[SimpleTimeNumberSystem] {
    final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
  }
  final override def createPoint(negative: Boolean, digits: Seq[Int]):
    TimePointBase[SimpleTimeNumberSystem] =
    new TimePointBase[SimpleTimeNumberSystem](negative, digits) with SimpleTimeNumberSystemMember
  final override def createInterval(negative: Boolean, digits: Seq[Int]):
    TimeIntervalBase[SimpleTimeNumberSystem] =
    new TimeIntervalBase[SimpleTimeNumberSystem](negative, digits) with SimpleTimeNumberSystemMember
  final object Interval extends IntervalCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
