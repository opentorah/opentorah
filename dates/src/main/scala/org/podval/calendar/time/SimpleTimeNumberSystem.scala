package org.podval.calendar.time

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.{IntervalCompanionBase, NumberSystemMember, PointCompanionBase}

// This exists so that TimeNumberSystem could be tested stand-alone.
// TODO "lift" tests to Calendar and eliminate?
class SimpleTimeNumberSystem extends TimeNumberSystem[SimpleTimeNumberSystem] {
  final override type Point = TimePointBase[SimpleTimeNumberSystem]
  final override type Interval = TimeIntervalBase[SimpleTimeNumberSystem]
  trait SimpleTimeNumberSystemMember extends NumberSystemMember[SimpleTimeNumberSystem] {
    final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
  }
  final override protected def createPoint(raw: RawNumber): TimePointBase[SimpleTimeNumberSystem] =
    new TimePointBase[SimpleTimeNumberSystem](raw) with SimpleTimeNumberSystemMember
  final override protected def createInterval(raw: RawNumber): TimeIntervalBase[SimpleTimeNumberSystem] =
    new TimeIntervalBase[SimpleTimeNumberSystem](raw) with SimpleTimeNumberSystemMember
  final object Interval extends IntervalCompanionBase[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanionBase[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
