package org.opentorah.times

// For stand-alone testing of Times.
object SimpleTimes extends Times {
  final override type Point = TimePointBase

  final override lazy val Point: PointCompanion =
    new PointCompanion {
      protected override def newNumber(digits: Seq[Int]): Point =
        new TimePointBase(digits) {
          final override def companion: PointCompanion = Point
        }
    }

  final override type Vector = TimeVectorBase

  final override lazy val Vector: VectorCompanion =
    new VectorCompanion {
      protected override def newNumber(digits: Seq[Int]): Vector =
        new TimeVectorBase(digits) {
          final override def companion: VectorCompanion = Vector
        }
    }
}
