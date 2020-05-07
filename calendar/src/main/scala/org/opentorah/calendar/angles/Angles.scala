package org.opentorah.calendar.angles

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, NumbersMember, PeriodicNumbers,
  PointCompanion, VectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  trait AnglesMember extends NumbersMember[Angles] {
    final override def numbers: Angles = Angles.this
  }

  final override type NumbersMemberType = AnglesMember

  final override type Vector = RotationAngle

  final type Rotation = Vector

  final override type VectorCompanionType = VectorCompanion[Angles] with AngleCompanion[Rotation]

  final override object Vector extends VectorCompanion[Angles] with AngleCompanion[Rotation] with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Vector =
      new Digits(digits) with Rotation with NumbersMemberType {
        final override def companion: VectorCompanionType = Vector
      }
  }

  final val Rotation = Vector

  final override type Point = PositionAngle

  final type Position = Point

  final override type PointCompanionType = PointCompanion[Angles] with AngleCompanion[Position]

  final override object Point extends PointCompanion[Angles] with AngleCompanion[Position] with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Point =
      new Digits(digits) with Position with NumbersMemberType {
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
