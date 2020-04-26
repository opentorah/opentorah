package org.opentorah.calendar.astronomy

import org.opentorah.calendar.angles.Angles.{Position, Rotation}
import org.opentorah.calendar.numbers.BigRational

object MoonCircuitPortion {

  // KH 17:10
  final def calculate(moonLongitudeTrue: Position): BigRational = {
    import Zodiac._
    def in(
      from1: Zodiac,
      fromDegrees: Int,
      to1: Zodiac,
      toDegrees: Int,
      from2: Zodiac,
      to2: Zodiac
    ): Boolean =
      ((from1.at(Rotation(fromDegrees)) <= moonLongitudeTrue) &&
       (moonLongitudeTrue < to1.at(Rotation(toDegrees)))) ||
      ((from2.at(Rotation(fromDegrees)) <= moonLongitudeTrue) &&
       (moonLongitudeTrue < to2.at(Rotation(toDegrees))))

    if (in(Aries ,  0, Aries , 20, Libra      , Libra      )) BigRational(2,  5) else
    if (in(Aries , 20, Taurus, 10, Libra      , Scorpio    )) BigRational(1,  3) else
    if (in(Taurus, 10, Taurus, 20, Scorpio    , Scorpio    )) BigRational(1,  4) else
    if (in(Taurus, 20, Taurus, 30, Scorpio    , Scorpio    )) BigRational(1,  5) else
    if (in(Gemini,  0, Gemini, 10, Sagittarius, Sagittarius)) BigRational(1,  6) else
    if (in(Gemini, 10, Gemini, 20, Sagittarius, Sagittarius)) BigRational(1, 12) else
    if (in(Gemini, 20, Gemini, 25, Sagittarius, Sagittarius)) BigRational(1, 24) else
    if (in(Gemini, 25, Cancer,  5, Sagittarius, Capricorn  )) BigRational.zero   else
    if (in(Cancer,  5, Cancer, 10, Capricorn  , Capricorn  )) BigRational(1, 24) else
    if (in(Cancer, 10, Cancer, 20, Capricorn  , Capricorn  )) BigRational(1, 12) else
    if (in(Cancer, 20, Cancer, 30, Capricorn  , Capricorn  )) BigRational(1,  6) else
    if (in(Leo   ,  0, Leo   , 10, Aquarius   , Aquarius   )) BigRational(1,  5) else
    if (in(Leo   , 10, Leo   , 20, Aquarius   , Aquarius   )) BigRational(1,  4) else
    if (in(Leo   , 20, Virgo , 10, Aquarius   , Pisces     )) BigRational(1,  3) else
    if (in(Virgo , 10, Virgo , 30, Pisces     , Pisces     )) BigRational(2,  5) else
      throw new IllegalArgumentException
  }
}
