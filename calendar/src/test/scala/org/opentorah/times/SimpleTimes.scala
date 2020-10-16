package org.opentorah.times

import org.opentorah.numbers.{NumbersMember, PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  trait SimpleTimesMember extends NumbersMember[SimpleTimes] {
    final override def numbers: SimpleTimes = SimpleTimes.this
  }

  final override type NumbersMemberType = SimpleTimesMember

  final override type Point = TimePointBase[SimpleTimes]

  final override type PointCompanionType = PointCompanion[SimpleTimes]

  final override object Point extends PointCompanionType with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Point =
      new TimePointBase[SimpleTimes](digits) with NumbersMemberType {
        final override def companion: PointCompanionType = Point
      }
  }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override type VectorCompanionType = VectorCompanion[SimpleTimes]

  final override object Vector extends VectorCompanionType with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Vector =
      new TimeVectorBase[SimpleTimes](digits) with NumbersMemberType {
        final override def companion: VectorCompanionType = Vector
      }
  }
}

object SimpleTimes extends SimpleTimes
