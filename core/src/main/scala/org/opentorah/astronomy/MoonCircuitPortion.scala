package org.opentorah.astronomy

import Angles.{Position, Rotation}
import Zodiac.*
import org.opentorah.numbers.BigRational
import MoonCircuitPortionHelper.or

// KH 17:10
object MoonCircuitPortion extends OrderedTable[Position, Position, BigRational](Seq(
  or(" 0°", Aries , Libra      , "20°", Aries , Libra      , "2/ 5"),
  or("20°", Aries , Libra      , "10°", Taurus, Scorpio    , "1/ 3"),
  or("10°", Taurus, Scorpio    , "20°", Taurus, Scorpio    , "1/ 4"),
  or("20°", Taurus, Scorpio    , "30°", Taurus, Scorpio    , "1/ 5"),
  or(" 0°", Gemini, Sagittarius, "10°", Gemini, Sagittarius, "1/ 6"),
  or("10°", Gemini, Sagittarius, "20°", Gemini, Sagittarius, "1/12"),
  or("20°", Gemini, Sagittarius, "25°", Gemini, Sagittarius, "1/24"),
  or("25°", Gemini, Sagittarius, " 5°", Cancer, Capricorn  , "   0"),
  or(" 5°", Cancer, Capricorn  , "10°", Cancer, Capricorn  , "1/24"),
  or("10°", Cancer, Capricorn  , "20°", Cancer, Capricorn  , "1/12"),
  or("20°", Cancer, Capricorn  , "30°", Cancer, Capricorn  , "1/ 6"),
  or(" 0°", Leo   , Aquarius   , "10°", Leo   , Aquarius   , "1/ 5"),
  or("10°", Leo   , Aquarius   , "20°", Leo   , Aquarius   , "1/ 4"),
  or("20°", Leo   , Aquarius   , "10°", Virgo , Pisces     , "1/ 3"),
  or("10°", Virgo , Pisces     , "30°", Virgo , Pisces     , "2/ 5")
).flatten*)(
  identity,
  BigRational(_)
):
  // TODO if the table is symmetric - why isn't Rambam exploiting the fact like in MoonLatitude?
  def calculate(moonLongitudeTrue: Position): BigRational = find(moonLongitudeTrue)

// Note: using this to make translation from the text literal
private object MoonCircuitPortionHelper:
  def or(
    from: String,
    from1: Zodiac,
    from2: Zodiac,
    to: String,
    to1: Zodiac,
    to2: Zodiac,
    value: String
  ): Seq[(Position, String)] = Seq(
    (from1.at(Rotation(from)), value),
    (from2.at(Rotation(from)), value)
  )
