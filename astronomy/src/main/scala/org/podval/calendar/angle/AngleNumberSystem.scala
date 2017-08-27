package org.podval.calendar.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitNumberSystem


trait AngleNumberSystem extends RangedHeadDigitNumberSystem[AngleNumberSystem] {
  final override type Interval = AngleBase

  final type Angle = Interval

  protected final override def createInterval(raw: RawNumber): Angle = new Angle(raw) {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final object Angle extends AngleCompanion

  final override type Point = AnglePointBase

  final type AnglePoint = Point

  protected final override def createPoint(raw: RawNumber): AnglePoint = new AnglePoint(raw) {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final object AnglePoint extends AnglePointCompanion

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
