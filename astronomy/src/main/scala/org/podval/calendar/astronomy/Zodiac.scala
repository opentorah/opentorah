package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

object Zodiac {

  // KH 11:9
  sealed abstract class Constellation(
    val nameLatin  : String,
    val nameEnglish: String,
    val nameHebrew : String,
    startDegrees   : Int
  ) {
    final def start: AnglePoint = AnglePoint(startDegrees)
    final def end: AnglePoint = (start + Angle(30)).canonical
    final def middle: AnglePoint = (start + Angle(15)).canonical

    final def contains(angle: AnglePoint): Boolean = (start <= angle) && (angle < end)

    final def at(angle: Angle): AnglePoint = {
      require(!angle.isNegative && (angle <= Angle(30)))
      start + angle
    }
  }

  case object Aries       extends Constellation("Aries"      , "Ram"        , "Tele"   ,   0)
  case object Taurus      extends Constellation("Taurus"     , "Bull"       , "Shor"   ,  30)
  case object Gemini      extends Constellation("Gemini"     , "Twins"      , "Toamim" ,  60)
  case object Cancer      extends Constellation("Cancer"     , "Crab"       , "Sartan" ,  90)
  case object Leo         extends Constellation("Leo"        , "Lion"       , "Aryeh"  , 120)
  case object Virgo       extends Constellation("Virgo"      , "Virgin"     , "Btulah" , 150)
  case object Libra       extends Constellation("Libra"      , "Balance"    , "Moznaim", 180)
  case object Scorpio     extends Constellation("Scorpio"    , "Scorpion"   , "Akrav"  , 210)
  case object Sagittarius extends Constellation("Sagittarius", "Archer"     , "Kashat" , 240)
  case object Capricorn   extends Constellation("Capricorn"  , "Goat"       , "Gdi"    , 270)
  case object Aquarius    extends Constellation("Aquarius"   , "Waterbearer", "Dli"    , 300)
  case object Pisces      extends Constellation("Pisces"     , "Fishes"     , "Dagim"  , 330)

  final val constellations: Seq[Constellation] = Seq(
    Aries, Taurus, Gemini, Cancer, Leo, Virgo,
    Libra, Scorpio, Sagittarius, Capricorn, Aquarius, Pisces)

  def fromAngle(rawAngle: AnglePoint): (Constellation, Angle) = {
    val angle: AnglePoint = rawAngle.canonical
    val constellation: Constellation = inConstellation(rawAngle)
    (constellation, angle - constellation.start)
  }

  def inConstellation(rawAngle: AnglePoint): Constellation = {
    val angle: AnglePoint = rawAngle.canonical
    constellations.find(_.contains(angle)).get
  }

  def in(rawAngle: AnglePoint, constellations: Set[Constellation]):Boolean = {
    val angle: AnglePoint = rawAngle.canonical
    constellations.exists(_.contains(angle))
  }
}
