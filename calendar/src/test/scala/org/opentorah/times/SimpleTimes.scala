package org.opentorah.times

import org.opentorah.numbers.{PointCompanion, VectorCompanion}

// For stand-alone testing of Times.
class SimpleTimes extends Times[SimpleTimes] {
  final override type Point = TimePointBase[SimpleTimes]

  final override lazy val Point: PointCompanion[SimpleTimes] =
    new PointCompanion[SimpleTimes] {
      override val numbers: SimpleTimes = SimpleTimes.this
      protected override def newNumber(digits: Seq[Int]): Point =
        new TimePointBase[SimpleTimes](digits) {
          override val numbers: SimpleTimes = SimpleTimes.this
          final override def companion: PointCompanion[SimpleTimes] = Point
        }
    }

  final override type Vector = TimeVectorBase[SimpleTimes]

  final override lazy val Vector: VectorCompanion[SimpleTimes] =
    new VectorCompanion[SimpleTimes] {
      override val numbers: SimpleTimes = SimpleTimes.this
      protected override def newNumber(digits: Seq[Int]): Vector =
        new TimeVectorBase[SimpleTimes](digits) {
          override val numbers: SimpleTimes = SimpleTimes.this
          final override def companion: VectorCompanion[SimpleTimes] = Vector
        }
    }
}

object SimpleTimes extends SimpleTimes
