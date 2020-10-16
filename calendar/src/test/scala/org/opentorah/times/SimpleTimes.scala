package org.opentorah.times

import org.opentorah.numbers.{PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  final override type Point = TimePointBase[SimpleTimes]

  final override type PointCompanionType = PointCompanion[SimpleTimes]

  final override lazy val Point = new PointCompanion[SimpleTimes](SimpleTimes.this) {
    protected override def newNumber(digits: Seq[Int]): Point =
      new TimePointBase[SimpleTimes](SimpleTimes.this, digits) {
        final override def companion: PointCompanionType = Point
      }
  }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override type VectorCompanionType = VectorCompanion[SimpleTimes]

  final override lazy val Vector = new VectorCompanion[SimpleTimes](SimpleTimes.this) {
    protected override def newNumber(digits: Seq[Int]): Vector =
      new TimeVectorBase[SimpleTimes](SimpleTimes.this, digits) {
        final override def companion: VectorCompanionType = Vector
      }
  }
}

object SimpleTimes extends SimpleTimes
