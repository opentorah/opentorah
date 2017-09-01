package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.{NumberSystemMember, PointCompanion, RangedHeadDigitNumberSystem}


trait AngleNumberSystem extends RangedHeadDigitNumberSystem[AngleNumberSystem] {
  final type Angle = AngleIntervalBase

  final override type Interval = Angle

  trait AngleNumberSystemMember extends NumberSystemMember[AngleNumberSystem] {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  protected final override def createInterval(raw: RawNumber): Angle =
    new Angle(raw) with AngleNumberSystemMember

  final object Angle extends AngleCompanion with AngleNumberSystemMember

  final override val Interval = Angle

  final type AnglePoint = AnglePointBase

  final override type Point = AnglePoint

  protected final override def createPoint(raw: RawNumber): AnglePoint =
    new AnglePoint(raw) with AngleNumberSystemMember

  final object AnglePoint extends PointCompanion[AngleNumberSystem]
    with AngleNumberCompanion[AnglePoint] with AngleNumberSystemMember

  final override val Point = AnglePoint

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
