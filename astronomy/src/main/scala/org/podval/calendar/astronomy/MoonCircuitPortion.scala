package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import org.podval.calendar.numbers.BigRational

object MoonCircuitPortion {

  // KH 17:10
  final def calculate(moonLongitudeTrue: AnglePoint): BigRational = {
    import Zodiac._
    def in(from: Constellation, fromDegrees: Int, to: Constellation, toDegrees: Int): Boolean =
      (from.at(Angle(fromDegrees)) <= moonLongitudeTrue) && (moonLongitudeTrue < to.at(Angle(toDegrees)))

    if (in(Aries ,  0, Aries , 20) || in(Libra      ,  0, Libra      , 20)) BigRational(2,  5) else
    if (in(Aries , 20, Taurus, 10) || in(Libra      , 20, Scorpio    , 10)) BigRational(1,  3) else
    if (in(Taurus, 10, Taurus, 20) || in(Scorpio    , 10, Scorpio    , 20)) BigRational(1,  4) else
    if (in(Taurus, 20, Taurus, 30) || in(Scorpio    , 20, Scorpio    , 30)) BigRational(1,  5) else
    if (in(Gemini,  0, Gemini, 10) || in(Sagittarius,  0, Sagittarius, 10)) BigRational(1,  6) else
    if (in(Gemini, 10, Gemini, 20) || in(Sagittarius, 10, Sagittarius, 20)) BigRational(1, 12) else
    if (in(Gemini, 20, Gemini, 25) || in(Sagittarius, 20, Sagittarius, 25)) BigRational(1, 24) else
    if (in(Gemini, 25, Cancer,  5) || in(Sagittarius, 25, Capricorn  ,  5)) BigRational.zero   else
    if (in(Cancer,  5, Cancer, 10) || in(Capricorn  ,  5, Capricorn  , 10)) BigRational(1, 24) else
    if (in(Cancer, 10, Cancer, 20) || in(Capricorn  , 10, Capricorn  , 20)) BigRational(1, 12) else
    if (in(Cancer, 20, Cancer, 30) || in(Capricorn  , 20, Capricorn  , 30)) BigRational(1,  6) else
    if (in(Leo   ,  0, Leo   , 10) || in(Aquarius   ,  0, Aquarius   , 10)) BigRational(1,  5) else
    if (in(Leo   , 10, Leo   , 20) || in(Aquarius   , 10, Aquarius   , 20)) BigRational(1,  4) else
    if (in(Leo   , 20, Virgo , 10) || in(Aquarius   , 20, Pisces     , 10)) BigRational(1,  3) else
    if (in(Virgo , 10, Virgo , 30) || in(Pisces     , 10, Pisces     , 30)) BigRational(2,  5) else
      throw new IllegalArgumentException
  }
}
