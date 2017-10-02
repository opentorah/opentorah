package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.AnglePoint
import org.podval.calendar.numbers.BigRational

object MoonLongitude3Portion {

  // KH 17:12
  final def calculate(moonLongitude3: AnglePoint): BigRational = {
    import Zodiac._
    def in(constellation1: Constellation, constellation2: Constellation): Boolean =
      Zodiac.in(moonLongitude3, Set(constellation1, constellation2))

    if (in(Pisces     , Aries ))  BigRational(1, 6) else
    if (in(Aquarius   , Taurus))  BigRational(1, 5) else
    if (in(Capricorn  , Gemini))  BigRational(1, 6) else
    if (in(Sagittarius, Cancer))  BigRational.zero  else
    if (in(Scorpio    , Leo   )) -BigRational(1, 5) else
    if (in(Libra      , Virgo )) -BigRational(1, 3) else
      throw new IllegalArgumentException
  }
}
