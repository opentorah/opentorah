package org.opentorah.angles

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, PeriodicNumbers, PointCompanion, VectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  final override type Vector = RotationAngle

  final type Rotation = Vector

  abstract class VectorCompanionType extends VectorCompanion[Angles] with AngleCompanion[Rotation]
  final override lazy val Vector: VectorCompanionType = new VectorCompanionType {
    override val numbers: Angles = Angles.this
    protected override def newNumber(digits: Digits): Vector = new RotationAngle(digits) {
      override val numbers: Angles = Angles.this
      final override def companion: VectorCompanionType = Vector
    }
  }

  final val Rotation = Vector

  final override type Point = PositionAngle

  final type Position = Point

  abstract class PointCompanionType extends PointCompanion[Angles] with AngleCompanion[Position]
  final override lazy val Point: PointCompanionType = new PointCompanionType {
      override val numbers: Angles = Angles.this
      protected override def newNumber(digits: Digits): Point =
        new PositionAngle(digits) {
          override val numbers: Angles = Angles.this
          final override def companion: PointCompanionType = Point
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
