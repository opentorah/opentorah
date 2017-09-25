package org.podval.calendar.angle

import org.podval.calendar.numbers.{IntervalCompanion, NumberSystemMember, PointCompanion,
  PeriodicNumberSystem, PeriodicPoint}

trait AngleNumberSystem extends PeriodicNumberSystem[AngleNumberSystem] {
  trait AngleNumberSystemMember extends NumberSystemMember[AngleNumberSystem] {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final override type Interval = AngleIntervalBase

  final type Angle = Interval

  final override def createInterval(digits: Seq[Int]): Interval =
    new Angle(digits) with AngleNumberSystemMember

  final override object Interval extends IntervalCompanion[AngleNumberSystem]
    with AngleNumberCompanion[Angle] with AngleNumberSystemMember

  final val Angle = Interval

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

  final override val signPartial: PartialFunction[Int, String] = {
    case 0 => "′"
    case 1 => "″"
    case 2 => "‴"
  }

  final override val defaultLength: Int = 3
}


object AngleNumberSystem extends AngleNumberSystem {
  import scala.language.implicitConversions

  implicit def angleToRadians(angle: Angle): Double = angle.toRadians
}
