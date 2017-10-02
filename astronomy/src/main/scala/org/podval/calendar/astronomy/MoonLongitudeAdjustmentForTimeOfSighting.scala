package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

// KH 14:5
object MoonLongitudeAdjustmentForTimeOfSighting {
  // TODO see notes in Moznaim Rambam
  // TODO sun longitude: mean or true?
  final def calculate(sun: AnglePoint): Angle = {
    import Zodiac._
    def in(from: AnglePoint, to: AnglePoint): Boolean = (from <= sun) && (sun < to)

    if (in(Pisces     .middle, Aries      .middle))  Angle(0)     else
    if (in(Aries      .middle, Gemini     .start ))  Angle(0, 15) else
    // TODO 15 in most editions!
    if (in(Gemini     .start , Leo        .start ))  Angle(0, 30) else
    if (in(Leo        .start , Virgo      .middle))  Angle(0, 15) else
    if (in(Virgo      .middle, Libra      .middle))  Angle(0)     else
    if (in(Libra      .middle, Sagittarius.start )) -Angle(0, 15) else
    // TODO 15 in most editions!
    if (in(Sagittarius.start , Aquarius   .start )) -Angle(0, 30) else
    if (in(Aquarius   .start , Pisces     .middle)) -Angle(0, 15) else
      throw new IllegalArgumentException
  }
}
