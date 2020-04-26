package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles.Position
import org.opentorah.calendar.numbers.BigRational

object MoonLongitude3Portion {

  // KH 17:12
  final def calculate(moonLongitude3: Position): BigRational = {
    import Zodiac._
    def in(zodiac1: Zodiac, zodiac2: Zodiac): Boolean =
      Zodiac.in(moonLongitude3, Set(zodiac1, zodiac2))

    if (in(Pisces     , Aries ))  BigRational(1, 6) else
    if (in(Aquarius   , Taurus))  BigRational(1, 5) else
    if (in(Capricorn  , Gemini))  BigRational(1, 6) else
    if (in(Sagittarius, Cancer))  BigRational.zero  else
    if (in(Scorpio    , Leo   )) -BigRational(1, 5) else
    if (in(Libra      , Virgo )) -BigRational(1, 3) else
      throw new IllegalArgumentException
  }
}
