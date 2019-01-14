package org.podval.calendar.times

import org.podval.calendar.numbers.{Digits, NumberCompanion, NumbersMember}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  trait SimpleTimesMember extends NumbersMember[SimpleTimes] {
    final override def numbers: SimpleTimes = SimpleTimes.this
  }

  final override type NumbersMemberType = SimpleTimesMember

  final override type Point = TimePointBase[SimpleTimes]

  final override type PointCompanionType = NumberCompanion[SimpleTimes, Point]

  final override object Point extends PointCompanionType with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Point =
      new Digits(digits) with Point with NumbersMemberType {
        final override def companion: PointCompanionType = Point
      }
  }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override type VectorCompanionType = NumberCompanion[SimpleTimes, Vector]

  final override object Vector extends VectorCompanionType with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Vector =
      new Digits(digits) with Vector with NumbersMemberType {
        final override def companion: VectorCompanionType = Vector
      }
  }
}

object SimpleTimes extends SimpleTimes
