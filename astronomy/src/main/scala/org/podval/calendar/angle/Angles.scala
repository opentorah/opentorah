package org.podval.calendar.angle

import org.podval.calendar.numbers.{VectorCompanion, NumberSystemMember, PointCompanion,
  PeriodicNumberSystem}

trait Angles extends PeriodicNumberSystem[Angles] {
  trait AnglesMember extends NumberSystemMember[Angles] {
    final override def numberSystem: Angles = Angles.this
  }

  final override type Vector = RotationBase

  final type Rotation = Vector

  final override def createVector(digits: Seq[Int]): Vector =
    new Rotation(digits) with AnglesMember

  final override object Vector extends VectorCompanion[Angles]
    with AngleCompanion[Rotation] with AnglesMember

  final val Rotation = Vector

  final override type Point = PositionBase

  final type Position = Point

  final override def createPoint(digits: Seq[Int]): Point =
    new Position(digits) with AnglesMember

  final override object Point extends PointCompanion[Angles]
    with AngleCompanion[Point] with AnglesMember

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
  import scala.language.implicitConversions

  // TODO use for rounding precision etc...
  object PositionIndex {
    final val MINUTES: Int = 0
    final val SECONDS: Int = 1
    final val THIRDS : Int = 2

    final val default = 3
  }

  implicit def angleToRadians(angle: Rotation): Double = angle.toRadians
}
