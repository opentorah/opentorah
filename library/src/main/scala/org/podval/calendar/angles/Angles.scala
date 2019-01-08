package org.podval.calendar.angles

import org.podval.calendar.numbers.{Digits, PeriodicVectorCompanion, NumbersMember,
  PeriodicPointCompanion, PeriodicNumbers, PointCompanion, VectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  trait AnglesMember extends NumbersMember[Angles] {
    final override def numbers: Angles = Angles.this
  }

  final override type Vector = RotationBase

  final type Rotation = Vector

  final override object Vector extends PeriodicVectorCompanion[Angles] with AngleCompanion[Rotation] with AnglesMember {
    override def apply(digits: Int*): Vector = new Digits(digits) with Rotation with AnglesMember {
      final override def companion: VectorCompanion[Angles] = Vector
    }
  }

  final val Rotation = Vector

  final override type Point = PositionBase

  final type Position = Point

  final override object Point extends PeriodicPointCompanion[Angles] with AngleCompanion[Point] with AnglesMember {
    override def apply(digits: Int*): Point = new Digits(digits) with Position with AnglesMember {
      final override def companion: PointCompanion[Angles] = Point
    }
  }

  final val Position = Point

  final override def headRange: Int = 360

  final override def range(position: Int): Int = 60

  final override def headSign: String = "°"

  import Angles.PositionIndex

  final override val signPartial: PartialFunction[Int, String] = {
    case PositionIndex.MINUTES => "′"
    case PositionIndex.SECONDS => "″"
    case PositionIndex.THIRDS  => "‴"
  }

  final override val defaultLength: Int = PositionIndex.default
}


object Angles extends Angles {
  object PositionIndex {
    final val MINUTES: Int = 0
    final val SECONDS: Int = 1
    final val THIRDS : Int = 2

    final val default = 3
  }
}
