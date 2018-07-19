package org.podval.calendar.angle

import org.podval.calendar.numbers.{VectorCompanion, NumberSystemMember, PointCompanion,
  PeriodicNumberSystem}

trait AngleNumberSystem extends PeriodicNumberSystem[AngleNumberSystem] {
  trait AngleNumberSystemMember extends NumberSystemMember[AngleNumberSystem] {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final override type Vector = AngleVectorBase

  final type Angle = Vector

  final override def createVector(digits: Seq[Int]): Vector =
    new Angle(digits) with AngleNumberSystemMember

  final override object Vector extends VectorCompanion[AngleNumberSystem]
    with AngleNumberCompanion[Angle] with AngleNumberSystemMember

  final val Angle = Vector

  final override type Point = AnglePointBase

  final type AnglePoint = Point

  final override def createPoint(digits: Seq[Int]): Point =
    new AnglePoint(digits) with AngleNumberSystemMember

  final override object Point extends PointCompanion[AngleNumberSystem]
    with AngleNumberCompanion[Point] with AngleNumberSystemMember

  final val AnglePoint = Point

  final override def headRange: Int = 360

  final override def range(position: Int): Int = 60

  final override def headSign: String = "°"

  import AngleNumberSystem.Position

  final override val signPartial: PartialFunction[Int, String] = {
    case Position.MINUTES => "′"
    case Position.SECONDS => "″"
    case Position.THIRDS  => "‴"
  }

  final override val defaultLength: Int = Position.default
}


object AngleNumberSystem extends AngleNumberSystem {
  import scala.language.implicitConversions

  object Position {
    final val MINUTES: Int = 0
    final val SECONDS: Int = 1
    final val THIRDS : Int = 2

    final val default = 3
  }

  implicit def angleToRadians(angle: Angle): Double = angle.toRadians
}
