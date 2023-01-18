package org.opentorah.astronomy

import Angles.Position
import Zodiac.*
import org.opentorah.numbers.BigRational
import MoonLongitude3PortionHelper.or

// KH 17:12
object MoonLongitude3Portion extends OrderedTable[Zodiac, Position, BigRational](Seq(
  or(Pisces     , Aries , " 1/6"),
  or(Aquarius   , Taurus, " 1/5"),
  or(Capricorn  , Gemini, " 1/6"),
  or(Sagittarius, Cancer, "   0"),
  or(Scorpio    , Leo   , "-1/5"),
  or(Libra      , Virgo , "-1/3")
).flatten*)(
  _.start,
  BigRational(_)
):
  // TODO if the table is symmetric - why isn't Rambam exploiting the fact like in MoonLatitude?
  def calculate(moonLongitude3: Position): BigRational = find(moonLongitude3)

// Note: using this to make translation from the text literal
private object MoonLongitude3PortionHelper:
  def or(zodiac1: Zodiac, zodiac2: Zodiac, value: String): Seq[(Zodiac, String)] = Seq(
    (zodiac1, value),
    (zodiac2, value)
  )
