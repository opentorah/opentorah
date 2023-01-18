package org.opentorah.astronomy

import org.opentorah.metadata.{HasName, Named, Names}
import Angles.{Position, Rotation}

// KH 11:9
enum Zodiac extends Named.ByLoader[Zodiac](loader = Zodiac, nameOverride = None), HasName.Enum, Ordered[Zodiac] derives CanEqual:
  final override def compare(that: Zodiac): Int = this.ordinal - that.ordinal // TODO should be derived...

  lazy val start: Position = Position(0) + Zodiac.size*ordinal
  lazy val end: Position = start + Zodiac.size
  lazy val middle: Position = start + Zodiac.halfSize

  def contains(angle: Position): Boolean = (start <= angle) && (angle < end)

  def at(angle: Rotation): Position =
    require(!angle.isNegative && (angle <= Zodiac.size))
    start + angle

  case Aries
  case Taurus
  case Gemini
  case Cancer
  case Leo
  case Virgo
  case Libra
  case Scorpio
  case Sagittarius
  case Capricorn
  case Aquarius
  case Pisces

object Zodiac extends Names.Loader[Zodiac]:

  val notherlyInclined: Seq[Zodiac] = Seq(Capricorn, Aquarius, Pisces, Aries, Taurus, Gemini)

  final override val valuesSeq: Seq[Zodiac] = values.toIndexedSeq

  private val (size: Rotation, halfSize: Rotation) =
    require(Angles.headRange % numberOfValues == 0)
    val sizeInDegrees: Int = Angles.headRange / numberOfValues
    require(sizeInDegrees % 2 == 0)
    (Rotation(sizeInDegrees), Rotation(sizeInDegrees / 2))

  def fromAngle(angle: Position): (Zodiac, Rotation) =
    val zodiac: Zodiac = forPosition(angle)
    (zodiac, angle - zodiac.start)

  def forPosition(angle: Position): Zodiac =
    valuesSeq.find(_.contains(angle)).get

