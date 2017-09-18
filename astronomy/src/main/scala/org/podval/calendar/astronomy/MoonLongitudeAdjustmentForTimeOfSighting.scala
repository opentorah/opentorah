package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

// KH 14:5
object MoonLongitudeAdjustmentForTimeOfSighting {
  // TODO see notes in Moznaim Rambam
  // TODO sun longitude: mean or true?
  final def adjustment(sun: AnglePoint): Angle = {
    def between(from: AnglePoint, to: AnglePoint): Boolean = (from <= sun) && (sun < to)
    if      (between(Zodiac.Pisces     .middle, Zodiac.Aries      .middle))  Angle(0)
    else if (between(Zodiac.Aries      .middle, Zodiac.Gemini     .start ))  Angle(0, 15)
    // TODO 15 in most editions!
    else if (between(Zodiac.Gemini     .start , Zodiac.Leo        .start ))  Angle(0, 30)
    else if (between(Zodiac.Leo        .start , Zodiac.Virgo      .middle))  Angle(0, 15)
    else if (between(Zodiac.Virgo      .middle, Zodiac.Libra      .middle))  Angle(0)
    else if (between(Zodiac.Libra      .middle, Zodiac.Sagittarius.start )) -Angle(0, 15)
    // TODO 15 in most editions!
    else if (between(Zodiac.Sagittarius.start , Zodiac.Aquarius   .start )) -Angle(0, 30)
    else if (between(Zodiac.Aquarius   .start , Zodiac.Pisces     .middle)) -Angle(0, 15)
    else throw new IllegalArgumentException
  }
}
