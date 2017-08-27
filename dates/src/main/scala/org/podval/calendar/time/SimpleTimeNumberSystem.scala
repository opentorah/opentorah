package org.podval.calendar.time

import org.podval.calendar.numbers.NumberSystem.RawNumber

// TODO "lift" tests to Calendar and eliminate?
// This exists so that TimeNumberSystem could be tested stand-alone.
// TODO add "companion" objects Interval etc.
class SimpleTimeNumberSystem extends TimeNumberSystem[SimpleTimeNumberSystem] {
  final override type Point = TimePointBase[SimpleTimeNumberSystem]
  final override type Interval = TimeIntervalBase[SimpleTimeNumberSystem]
  final override protected def createPoint(raw: RawNumber): TimePointBase[SimpleTimeNumberSystem] =
    new TimePointBase[SimpleTimeNumberSystem](raw) {
      final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
    }
  final override protected def createInterval(raw: RawNumber): TimeIntervalBase[SimpleTimeNumberSystem] =
    new TimeIntervalBase[SimpleTimeNumberSystem](raw) {
      final override def numberSystem: SimpleTimeNumberSystem = SimpleTimeNumberSystem.this
    }
}


object SimpleTimeNumberSystem extends SimpleTimeNumberSystem
