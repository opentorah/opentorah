package org.opentorah.angles

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, NumbersMember, PeriodicNumbers,
  PointCompanion, VectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  trait AnglesMember extends NumbersMember[Angles]

  final override type Vector = RotationAngle

  final type Rotation = Vector

  final override type VectorCompanionType = VectorCompanion[Angles] with AngleCompanion[Rotation]

  final override lazy val Vector = new VectorCompanion[Angles](Angles.this) with AngleCompanion[Rotation] {
    protected override def newNumber(digits: Digits): Vector =
      new RotationAngle(Angles.this, digits) {
        final override def companion: VectorCompanionType = Vector
      }
  }

  final val Rotation = Vector

  final override type Point = PositionAngle

  final type Position = Point

  final override type PointCompanionType = PointCompanion[Angles] with AngleCompanion[Position]

  final override lazy val Point = new PointCompanion[Angles](Angles.this) with AngleCompanion[Position] {
    protected override def newNumber(digits: Digits): Point =
      new PositionAngle(Angles.this, digits) {
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
