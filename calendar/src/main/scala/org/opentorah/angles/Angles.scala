package org.opentorah.angles

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, NumbersMember, PeriodicNumbers,
  PointCompanion, VectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  final override type Vector = RotationAngle

  final type Rotation = Vector

  final override lazy val Vector: VectorCompanion[Angles] with AngleCompanion[Rotation] =
    new VectorCompanion[Angles] with AngleCompanion[Rotation] {
      override val numbers: Angles = Angles.this
      protected override def newNumber(digits: Digits): Vector =
        new RotationAngle(digits) {
          override val numbers: Angles = Angles.this
          final override def companion: VectorCompanion[Angles] with AngleCompanion[Rotation] = Vector
        }
    }

  final val Rotation = Vector

  final override type Point = PositionAngle

  final type Position = Point

  final override lazy val Point: PointCompanion[Angles] with AngleCompanion[Position] =
    new PointCompanion[Angles] with AngleCompanion[Position] {
      override val numbers: Angles = Angles.this
      protected override def newNumber(digits: Digits): Point =
        new PositionAngle(digits) {
          override val numbers: Angles = Angles.this
          final override def companion: PointCompanion[Angles] with AngleCompanion[Position] = Point
        }
    }

  final val Position = Point

  final override def headRange: Int = 360

  final override val maxLength: Int = 10

  final override def range(position: Int): Int = 60

  object Digit extends DigitsDescriptor {
    object DEGREES extends DigitBase("°")
    object MINUTES extends DigitBase("′")
    object SECONDS extends DigitBase("″")
    object THIRDS  extends DigitBase("‴")

    override val values: Seq[Digit] = Seq(DEGREES, MINUTES, SECONDS, THIRDS)
  }
}

object Angles extends Angles
