package org.podval.calendar.times

import org.podval.calendar.numbers.{Digits, NumbersMember, PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  trait SimpleTimesMember extends NumbersMember[SimpleTimes] {
    final override def numbers: SimpleTimes = SimpleTimes.this
  }

  final override type Point = TimePointBase[SimpleTimes]

  final override object Point extends PointCompanion[SimpleTimes] with SimpleTimesMember {
    override def apply(digits: Int*): Point = new Digits(digits) with TimePointBase[SimpleTimes] {
      final override def companion: PointCompanion[SimpleTimes] = Point
    }
  }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override object Vector extends VectorCompanion[SimpleTimes] with SimpleTimesMember {
    override def apply(digits: Int*): Vector = new Digits(digits) with TimeVectorBase[SimpleTimes] {
      final override def companion: VectorCompanion[SimpleTimes] = Vector
    }
  }
}


object SimpleTimes extends SimpleTimes
