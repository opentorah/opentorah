package org.podval.calendar.time

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.{IntervalCompanion, NumberSystemMember, PointCompanion}

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
  final object Interval extends IntervalCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
  final object Point extends PointCompanion[SimpleTimeNumberSystem] with SimpleTimeNumberSystemMember
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
