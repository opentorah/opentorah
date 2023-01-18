package org.opentorah.astronomy

import org.opentorah.numbers.{Digits, Numbers}

object Angles extends Numbers.Periodic:

  enum AnglesDigit(override val sign: String) extends Digit(sign):
    case DEGREES extends AnglesDigit("°")
    case MINUTES extends AnglesDigit("′")
    case SECONDS extends AnglesDigit("″")
    case THIRDS  extends AnglesDigit("‴")

  override type DigitType = AnglesDigit

  override type DigitCompanionType = AnglesDigit.type

  override def Digit: DigitCompanionType = AnglesDigit

  override protected def digitDescriptors: Array[AnglesDigit] = AnglesDigit.values

  override protected def digitSignDefault: String = ","

  trait Angle[N <: Angle[N]] extends Number[N]:
    this: N =>
    final def degrees: Int = get(Digit.DEGREES)
    final def degrees(value: Int): N = set(Digit.DEGREES, value)
    final def roundToDegrees: N = roundTo(Digit.DEGREES)
    final def minutes: Int = get(Digit.MINUTES)
    final def minutes(value: Int): N = set(Digit.MINUTES, value)
    final def roundToMinutes: N = roundTo(Digit.MINUTES)
    final def seconds: Int = get(Digit.SECONDS)
    final def seconds(value: Int): N = set(Digit.SECONDS, value)
    final def roundToSeconds: N = roundTo(Digit.SECONDS)
    final def thirds: Int  = get(Digit.THIRDS)
    final def thirds(value: Int): N = set(Digit.THIRDS, value)
    final def roundToThirds: N = roundTo(Digit.THIRDS)
    final def toRadians: Double = math.toRadians(toDegrees)
    final def toDegrees: Double = toDouble

  sealed trait AngleCompanion[N <: Angle[N]] extends NumberCompanion[N]:
    final def fromRadians(value: Double, length: Int): N = fromDegrees(math.toDegrees(value), length)

    final def fromDegrees(value: Double, length: Int): N = fromDouble(value, length)

  final class RotationAngle(digits: Digits) extends VectorNumber(digits), Angle[RotationAngle]

  override type Vector = RotationAngle

  type Rotation = Vector

  final class RotationCompanion extends VectorCompanion, AngleCompanion[Rotation]

  override protected def newVector(digits: Digits): Vector = RotationAngle(digits)

  override type VectorCompanionType = RotationCompanion

  override protected def createVectorCompanion: VectorCompanionType = new RotationCompanion

  val Rotation: RotationCompanion = Vector

  final class PositionAngle(digits: Digits) extends PointNumber(digits), Angle[PositionAngle]

  override type Point = PositionAngle

  type Position = Point

  final class PositionCompanion extends PointCompanion, AngleCompanion[Position]

  override protected def newPoint(digits: Digits): Point = PositionAngle(digits)

  override type PointCompanionType = PositionCompanion

  override protected def createPointCompanion: PointCompanionType = new PositionCompanion

  val Position: PositionCompanion = Point

  override def headRange: Int = 360

  override val maxLength: Int = 10

  override def range(position: Int): Int = 60
