package org.podval.calendar.angles

import org.podval.calendar.numbers.{Digits, DigitsDescriptor, NumbersMember, PeriodicNumbers, PeriodicVectorCompanion}

trait Angles extends PeriodicNumbers[Angles] {
  trait AnglesMember extends NumbersMember[Angles] {
    final override def numbers: Angles = Angles.this
  }

  final override type NumbersMemberType = AnglesMember

  final override type Vector = RotationBase

  final type Rotation = Vector

  final override type VectorCompanionType = PeriodicVectorCompanion[Angles] with AngleCompanion[Rotation]

  final override object Vector extends PeriodicVectorCompanion[Angles] with AngleCompanion[Rotation] with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Vector =
      new Digits(digits) with Rotation with NumbersMemberType {
        final override def companion: VectorCompanionType = Vector
      }
  }

  final val Rotation = Vector

  final override type Point = PositionBase

  final type Position = Point

  final override type PointCompanionType = AngleCompanion[Position]

  final override object Point extends PointCompanionType with NumbersMemberType {
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

    override val values: Seq[DigitsDescriptor.Digit] = Seq(DEGREES, MINUTES, SECONDS, THIRDS)
  }
}

object Angles extends Angles
