package org.podval.calendar.astronomy.angle

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.numbers.RangedHeadDigitNumberSystem


class AngleNumberSystem extends {

  // TODO rename
  private val max_length = 10

  final override val signs: List[String] =
    List("°", "′", "″", "‴") ++ List.empty.padTo(max_length - 4, ",")

  final override val ranges: List[Int] = List.empty.padTo(max_length - 1, 60)

  final override val headRange: Int = 360

} with RangedHeadDigitNumberSystem[AngleNumberSystem] {
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
}


object AngleNumberSystem extends AngleNumberSystem
