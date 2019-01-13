package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles
import org.podval.calendar.angles.Angles.{Position, Rotation}
import org.podval.judaica.metadata.{Named, NamedCompanion, Names}

// KH 11:9
sealed abstract class Zodiac extends Named {
  final override def names: Names = Zodiac.toNames(this)

  final lazy val start: Position = Position(0) + Zodiac.size*Zodiac.indexOf(this)
  final lazy val end: Position = start + Zodiac.size
  final lazy val middle: Position = start + Zodiac.halfSize

  final def contains(angle: Position): Boolean = (start <= angle) && (angle < end)

  final def at(angle: Rotation): Position = {
    require(!angle.isNegative && (angle <= Zodiac.size))
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

  private val (size: Rotation, halfSize: Rotation) = {
    require(Angles.headRange % numberOfValues == 0)
    val sizeInDegrees: Int = Angles.headRange / numberOfValues
    require(sizeInDegrees % 2 == 0)
    (Rotation(sizeInDegrees), Rotation(sizeInDegrees / 2))
  }

  def fromAngle(angle: Position): (Zodiac, Rotation) = {
    val zodiac: Zodiac = inZodiac(angle)
    (zodiac, angle - zodiac.start)
  }

  def inZodiac(angle: Position): Zodiac =
    values.find(_.contains(angle)).get

  def in(angle: Position, zodiacs: Set[Zodiac]):Boolean =
    zodiacs.exists(_.contains(angle))
}
