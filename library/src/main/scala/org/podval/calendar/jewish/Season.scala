package org.podval.calendar.jewish

sealed class Season(val numberInYear: Int, val name: String)

object Season {
  // tkufos KH 9:3, 10:3

  // sun enters Tele (Aries)
  final case object TkufasNisan extends Season(3, "Tkufas Nisan")
  final def SpringEquinox: Season = TkufasNisan
  final def VernalEquinox: Season = SpringEquinox

  // sun enters Sarton (Cancer)
  final case object TkufasTammuz extends Season(4, "Tkufas Tammuz")
  final def SummerSolstice: Season = TkufasTammuz
  final def EstivalSolstice: Season = SummerSolstice

  // sun enters Moznaim (Libra)
  final case object TkufasTishrei extends Season(1, "Tkufas Tishrei")
  final def AutumnalEquinox: Season = TkufasTishrei
  final def FallEquinox: Season = AutumnalEquinox

  // sun enters Gdi (Capricorn)
  final case object TkufasTeves extends Season(2, "Tkufas Teves")
  final def WinterSolstice: Season = TkufasTeves
  final def SouthernSolstice: Season = WinterSolstice

  final val values: Seq[Season] = Seq(TkufasTishrei, TkufasTeves, TkufasNisan, TkufasTammuz)

  final val numberOf: Int = values.length

  trait ForYear {
    def seasonForYear(season: Season, year: Jewish.Year): Jewish.Moment
  }
}
