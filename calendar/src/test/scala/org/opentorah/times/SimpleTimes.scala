package org.opentorah.times

import org.opentorah.numbers.{PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  final override type Point = TimePointBase[SimpleTimes]

  final override lazy val Point: PointCompanion[SimpleTimes] =
    new PointCompanion[SimpleTimes](SimpleTimes.this) {
      protected override def newNumber(digits: Seq[Int]): Point =
        new TimePointBase[SimpleTimes](SimpleTimes.this, digits) {
          final override def companion: PointCompanion[SimpleTimes] = Point
        }
    }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override lazy val Vector: VectorCompanion[SimpleTimes] =
    new VectorCompanion[SimpleTimes](SimpleTimes.this) {
      protected override def newNumber(digits: Seq[Int]): Vector =
        new TimeVectorBase[SimpleTimes](SimpleTimes.this, digits) {
          final override def companion: VectorCompanion[SimpleTimes] = Vector
        }
    }
}

object SimpleTimes extends SimpleTimes
