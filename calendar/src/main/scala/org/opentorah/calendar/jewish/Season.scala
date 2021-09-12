package org.opentorah.calendar.jewish

import org.opentorah.metadata.{Named, NamedCompanion, Names}

// "abstract" is here to help with the exhaustiveness check.
sealed abstract class Season(override val name: String) extends Named:
  final lazy val numberInYear: Int = Season.indexOf(this) + 1

  final override def names: Names = Season.toNames(this)

object Season extends NamedCompanion:
  override type Key = Season

  // tkufos KH 9:3, 10:3

  // sun enters Tele (Aries)
  case object TkufasNisan extends Season("Spring Equinox")
  final def SpringEquinox: Season = TkufasNisan
  final def VernalEquinox: Season = SpringEquinox

  // sun enters Sarton (Cancer)
  case object TkufasTammuz extends Season("Summer Solstice")
  final def SummerSolstice: Season = TkufasTammuz
  final def EstivalSolstice: Season = SummerSolstice

  // sun enters Moznaim (Libra)
  case object TkufasTishrei extends Season("Autumnal Equinox")
  final def AutumnalEquinox: Season = TkufasTishrei
  final def FallEquinox: Season = AutumnalEquinox

  // sun enters Gdi (Capricorn)
  case object TkufasTeves extends Season("Winter Solstice")
  final def WinterSolstice: Season = TkufasTeves
  final def SouthernSolstice: Season = WinterSolstice

  final override val values: Seq[Season] = Seq(TkufasTishrei, TkufasTeves, TkufasNisan, TkufasTammuz)

  trait ForYear:
    def seasonForYear(season: Season, year: Jewish.Year): Jewish.Moment
