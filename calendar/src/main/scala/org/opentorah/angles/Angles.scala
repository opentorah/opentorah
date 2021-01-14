package org.opentorah.angles

import org.opentorah.numbers.{Digit, Digits, DigitsDescriptor, PeriodicNumbers}

trait Angles extends PeriodicNumbers {

  trait Angle[N <: Angle[N]] extends Number[N] { this: N =>
    def degrees: Int = get(Digit.DEGREES)

    def degrees(value: Int): N = set(Digit.DEGREES, value)

    def roundToDegrees: N = roundTo(Digit.DEGREES)

    def minutes: Int = get(Digit.MINUTES)

    def minutes(value: Int): N = set(Digit.MINUTES, value)

    def roundToMinutes: N = roundTo(Digit.MINUTES)

    def seconds: Int = get(Digit.SECONDS)

    def seconds(value: Int): N = set(Digit.SECONDS, value)

    def roundToSeconds: N = roundTo(Digit.SECONDS)

    def thirds: Int  = get(Digit.THIRDS)

    def thirds(value: Int): N = set(Digit.THIRDS, value)

    def roundToThirds: N = roundTo(Digit.THIRDS)

    def toRadians: Double = math.toRadians(toDegrees)

    def toDegrees: Double = toDouble
  }

  trait AngleCompanion[N <: Angle[N]] extends NumberCompanion[N] { this: NumberCompanion[N] =>
    final def fromRadians(value: Double, length: Int): N = fromDegrees(math.toDegrees(value), length)

    final def fromDegrees(value: Double, length: Int): N = fromDouble(value, length)
  }

  abstract class RotationAngle(digits: Digits) extends VectorNumber(digits) with Angle[RotationAngle]

  final override type Vector = RotationAngle

  final type Rotation = Vector

  abstract class VectorCompanionType extends VectorCompanion with AngleCompanion[Rotation]
  final override lazy val Vector: VectorCompanionType = new VectorCompanionType {
    protected override def newNumber(digits: Digits): Vector = new RotationAngle(digits) {
      final override def companion: VectorCompanionType = Vector
    }
  }

  final val Rotation = Vector

  abstract class PositionAngle(digits: Digits) extends PointNumber(digits) with Angle[PositionAngle]

  final override type Point = PositionAngle

  final type Position = Point

  abstract class PointCompanionType extends PointCompanion with AngleCompanion[Position]

  final override lazy val Point: PointCompanionType = new PointCompanionType {
      protected override def newNumber(digits: Digits): Point =
        new PositionAngle(digits) {
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
