package org.podval.calendar.angles

import org.podval.calendar.numbers.{Digits, DigitsDescriptor, PeriodicVectorCompanion, NumbersMember,
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

  object Digit extends DigitsDescriptor {
    object DEGREES extends DigitBase("°")
    object MINUTES extends DigitBase("′")
    object SECONDS extends DigitBase("″")
    object THIRDS  extends DigitBase("‴")

    override val values: Seq[DigitsDescriptor.Digit] = Seq(DEGREES, MINUTES, SECONDS, THIRDS)
  }

  final override def headRange: Int = 360

  final override def range(position: Int): Int = 60

  final override val maxLength: Int = 10
}

object Angles extends Angles
