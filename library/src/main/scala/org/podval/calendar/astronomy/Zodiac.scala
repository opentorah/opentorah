package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Position, Rotation}
import org.podval.judaica.metadata.{Named, NamedCompanion, Names}

// KH 11:9
sealed abstract class Zodiac extends Named {
  final override def names: Names = Zodiac.toNames(this)

  final lazy val startDegrees: Int = 30*Zodiac.indexOf(this)

  final def start: Position = Position(startDegrees)
  final def end: Position = (start + Rotation(30)).canonical
  final def middle: Position = (start + Rotation(15)).canonical

  final def contains(angle: Position): Boolean = (start <= angle) && (angle < end)

  final def at(angle: Rotation): Position = {
    require(!angle.isNegative && (angle <= Rotation(30)))
    start + angle
  }
}

object Zodiac extends NamedCompanion {
  override type Key = Zodiac

  case object Aries       extends Zodiac
  case object Taurus      extends Zodiac
  case object Gemini      extends Zodiac
  case object Cancer      extends Zodiac
  case object Leo         extends Zodiac
  case object Virgo       extends Zodiac
  case object Libra       extends Zodiac
  case object Scorpio     extends Zodiac
  case object Sagittarius extends Zodiac
  case object Capricorn   extends Zodiac
  case object Aquarius    extends Zodiac
  case object Pisces      extends Zodiac

  final override val values: Seq[Zodiac] = Seq(
    Aries, Taurus, Gemini, Cancer, Leo, Virgo,
    Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)

  def fromAngle(rawAngle: Position): (Zodiac, Rotation) = {
    val angle: Position = rawAngle.canonical
    val zodiac: Zodiac = inZodiac(rawAngle)
    (zodiac, angle - zodiac.start)
  }

  def inZodiac(rawAngle: Position): Zodiac = {
    val angle: Position = rawAngle.canonical
    values.find(_.contains(angle)).get
  }

  def in(rawAngle: Position, zodiacs: Set[Zodiac]):Boolean = {
    val angle: Position = rawAngle.canonical
    zodiacs.exists(_.contains(angle))
  }
}
