package org.opentorah.astronomy

import Angles.Position
import org.opentorah.numbers.BigRational

object MoonLongitude3Portion:

  // KH 17:12
  final def calculate(moonLongitude3: Position): BigRational =
    import Zodiac.*
    def in(zodiac1: Zodiac, zodiac2: Zodiac): Boolean =
      Zodiac.in(moonLongitude3, Set(zodiac1, zodiac2))

    if in(Pisces     , Aries ) then  BigRational(1, 6) else
    if in(Aquarius   , Taurus) then  BigRational(1, 5) else
    if in(Capricorn  , Gemini) then  BigRational(1, 6) else
    if in(Sagittarius, Cancer) then  BigRational.zero  else
    if in(Scorpio    , Leo   ) then -BigRational(1, 5) else
    if in(Libra      , Virgo ) then -BigRational(1, 3) else
      throw IllegalArgumentException()
