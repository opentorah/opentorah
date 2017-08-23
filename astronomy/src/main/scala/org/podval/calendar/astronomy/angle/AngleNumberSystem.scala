package org.podval.calendar.astronomy.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitNumberSystem


trait AngleNumberSystem extends RangedHeadDigitNumberSystem[AngleNumberSystem] {
  final override type Interval = AngleBase

  final type Angle = Interval

  final override def createInterval(raw: RawNumber): Angle = new Angle(raw) {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final object Angle extends AngleCompanion

  final override type Point = AnglePointBase

  final type AnglePoint = Point

  final override def createPoint(raw: RawNumber): AnglePoint = new AnglePoint(raw) {
    final override def numberSystem: AngleNumberSystem = AngleNumberSystem.this
  }

  final object AnglePoint extends AnglePointCompanion

  final override def headRange: Int = 360

  final override def range(position: Int): Int = 60

  final override def headSign: String = "°"

  final override def sign(position: Int): Option[String] = position match {
    case 0 => Some("′")
    case 1 => Some("″")
    case 2 => Some("‴")
    case _ => None
  }
}


object AngleNumberSystem extends AngleNumberSystem {
  import scala.language.implicitConversions

  implicit def angleToRadians(angle: Angle): Double = angle.toRadians
}
