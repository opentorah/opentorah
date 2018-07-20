package org.podval.calendar.angles

import org.podval.calendar.numbers.{Digits, VectorCompanion, NumbersMember,
  PointCompanion, PeriodicNumbers}

trait Angles extends PeriodicNumbers[Angles] {
  trait AnglesMember extends NumbersMember[Angles] {
    final override def numberSystem: Angles = Angles.this
  }

  final override type Vector = RotationBase

  final type Rotation = Vector

  final override def createVector(digits: Seq[Int]): Vector =
    new Digits(digits) with Rotation with AnglesMember

  final override object Vector extends VectorCompanion[Angles]
    with AngleCompanion[Rotation] with AnglesMember

  final val Rotation = Vector

  final override type Point = PositionBase

  final type Position = Point

  final override def createPoint(digits: Seq[Int]): Point =
    new Digits(digits) with Position with AnglesMember

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

  object PositionIndex {
    final val MINUTES: Int = 0
    final val SECONDS: Int = 1
    final val THIRDS : Int = 2

    final val default = 3
  }

  implicit def angleToRadians(angle: Rotation): Double = angle.toRadians
}
