package org.opentorah.calendar.jewish

import org.opentorah.astronomy.Zodiac
import org.opentorah.metadata.{HasName, Named, Names}

enum Season(name: String, val sunEnters: Zodiac)
  extends Named.ByLoader[Season](loader = Season, nameOverride = Some(name)), HasName.Enum derives CanEqual:
  lazy val numberInYear: Int = ordinal + 1

  // tkufos KH 9:3, 10:3
  case TkufasNisan   extends Season("Spring Equinox"  , Zodiac.Aries    )
  case TkufasTammuz  extends Season("Summer Solstice" , Zodiac.Cancer   )
  case TkufasTishrei extends Season("Autumnal Equinox", Zodiac.Libra    )
  case TkufasTeves   extends Season("Winter Solstice" , Zodiac.Capricorn)

object Season extends Names.Loader[Season]:
  def SpringEquinox   : Season = TkufasNisan
  def VernalEquinox   : Season = SpringEquinox
  def SummerSolstice  : Season = TkufasTammuz
  def EstivalSolstice : Season = SummerSolstice
  def AutumnalEquinox : Season = TkufasTishrei
  def FallEquinox     : Season = AutumnalEquinox
  def WinterSolstice  : Season = TkufasTeves
  def SouthernSolstice: Season = WinterSolstice

  override val valuesSeq: Seq[Season] = values.toIndexedSeq

  trait ForYear:
    def seasonForYear(season: Season, year: Jewish.Year): Jewish.Moment
