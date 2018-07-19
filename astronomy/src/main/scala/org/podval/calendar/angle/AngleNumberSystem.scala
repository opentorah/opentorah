package org.podval.calendar.angle

import org.podval.calendar.numbers.{VectorCompanion, NumberSystemMember, PointCompanion,
  PeriodicNumberSystem}

trait AngleNumberSystem extends PeriodicNumberSystem[AngleNumberSystem] {
  trait AngleNumberSystemMember extends NumberSystemMember[AngleNumberSystem] {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final override type Vector = AngleVectorBase

  // TODO rename Movement? Rotation?
  final type Angle = Vector

  final override def createVector(digits: Seq[Int]): Vector =
    new Angle(digits) with AngleNumberSystemMember

  final override object Vector extends VectorCompanion[AngleNumberSystem]
    with AngleNumberCompanion[Angle] with AngleNumberSystemMember

  final val Angle = Vector

  final override type Point = PositionBase

  final type Position = Point

  final override def createPoint(digits: Seq[Int]): Point =
    new Position(digits) with AngleNumberSystemMember

  final override object Point extends PointCompanion[AngleNumberSystem]
    with AngleNumberCompanion[Point] with AngleNumberSystemMember

  final val Position = Point

  final override def headRange: Int = 360

  final override def range(position: Int): Int = 60

  final override def headSign: String = "°"

  import AngleNumberSystem.PositionIndex

  final override val signPartial: PartialFunction[Int, String] = {
    case PositionIndex.MINUTES => "′"
    case PositionIndex.SECONDS => "″"
    case PositionIndex.THIRDS  => "‴"
  }

  final override val defaultLength: Int = PositionIndex.default
}


object AngleNumberSystem extends AngleNumberSystem {
  import scala.language.implicitConversions

  // TODO use for rounding precision etc...
  object PositionIndex {
    final val MINUTES: Int = 0
    final val SECONDS: Int = 1
    final val THIRDS : Int = 2

    final val default = 3
  }

  implicit def angleToRadians(angle: Angle): Double = angle.toRadians
}
