package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

// KH 11:9
sealed abstract class Zodiac(
  val nameLatin   : String,
  val nameEnglish : String,
  val nameHebrew  : String,
  val startDegrees: Int)
{
  final def start: AnglePoint = AnglePoint(startDegrees)
  final def end: AnglePoint = (start + Angle(30)).canonical
  final def middle: AnglePoint = (start + Angle(15)).canonical

  final def contains(angle: AnglePoint): Boolean = (start <= angle) && (angle < end)

  final def at(angle: Angle): AnglePoint = {
    require(!angle.isNegative && (angle <= Angle(30)))
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

  def fromAngle(rawAngle: AnglePoint): (Zodiac, Angle) = {
    val angle: AnglePoint = rawAngle.canonical
    val zodiac: Zodiac = inZodiac(rawAngle)
    (zodiac, angle - zodiac.start)
  }

  def inZodiac(rawAngle: AnglePoint): Zodiac = {
    val angle: AnglePoint = rawAngle.canonical
    all.find(_.contains(angle)).get
  }

  def in(rawAngle: AnglePoint, zodiacs: Set[Zodiac]):Boolean = {
    val angle: AnglePoint = rawAngle.canonical
    zodiacs.exists(_.contains(angle))
  }
}
