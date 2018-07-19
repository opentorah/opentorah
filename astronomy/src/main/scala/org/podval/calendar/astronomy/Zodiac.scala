package org.podval.calendar.astronomy

import org.podval.calendar.angle.Angles.{Rotation, Position}

// KH 11:9
sealed abstract class Zodiac(
  val nameLatin   : String,
  val nameEnglish : String,
  val nameHebrew  : String,
  val startDegrees: Int)
{
  final def start: Position = Position(startDegrees)
  final def end: Position = (start + Rotation(30)).canonical
  final def middle: Position = (start + Rotation(15)).canonical

  final def contains(angle: Position): Boolean = (start <= angle) && (angle < end)

  final def at(angle: Rotation): Position = {
    require(!angle.isNegative && (angle <= Rotation(30)))
    start + angle
  }
}

object Zodiac {
  case object Aries       extends Zodiac("Aries"      , "Ram"        , "Tele"   ,   0)
  case object Taurus      extends Zodiac("Taurus"     , "Bull"       , "Shor"   ,  30)
  case object Gemini      extends Zodiac("Gemini"     , "Twins"      , "Toamim" ,  60)
  case object Cancer      extends Zodiac("Cancer"     , "Crab"       , "Sartan" ,  90)
  case object Leo         extends Zodiac("Leo"        , "Lion"       , "Aryeh"  , 120)
  case object Virgo       extends Zodiac("Virgo"      , "Virgin"     , "Btulah" , 150)
  case object Libra       extends Zodiac("Libra"      , "Balance"    , "Moznaim", 180)
  case object Scorpio     extends Zodiac("Scorpio"    , "Scorpion"   , "Akrav"  , 210)
  case object Sagittarius extends Zodiac("Sagittarius", "Archer"     , "Kashat" , 240)
  case object Capricorn   extends Zodiac("Capricorn"  , "Goat"       , "Gdi"    , 270)
  case object Aquarius    extends Zodiac("Aquarius"   , "Waterbearer", "Dli"    , 300)
  case object Pisces      extends Zodiac("Pisces"     , "Fishes"     , "Dagim"  , 330)

  final val all: Seq[Zodiac] = Seq(
    Aries, Taurus, Gemini, Cancer, Leo, Virgo,
    Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)

  def fromAngle(rawAngle: Position): (Zodiac, Rotation) = {
    val angle: Position = rawAngle.canonical
    val zodiac: Zodiac = inZodiac(rawAngle)
    (zodiac, angle - zodiac.start)
  }

  def inZodiac(rawAngle: Position): Zodiac = {
    val angle: Position = rawAngle.canonical
    all.find(_.contains(angle)).get
  }

  def in(rawAngle: Position, zodiacs: Set[Zodiac]):Boolean = {
    val angle: Position = rawAngle.canonical
    zodiacs.exists(_.contains(angle))
  }
}
