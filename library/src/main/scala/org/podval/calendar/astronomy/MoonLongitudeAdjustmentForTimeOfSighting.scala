package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Position, Rotation}

// KH 14:5
object MoonLongitudeAdjustmentForTimeOfSighting {
  final def calculate(sun: Position): Rotation = {
    import Zodiac._
    def in(from: Position, to: Position): Boolean = (from <= sun) && (sun < to)

    if (in(Pisces     .middle, Aries      .middle))  Rotation(0)     else
    if (in(Aries      .middle, Gemini     .start ))  Rotation(0, 15) else
    if (in(Gemini     .start , Leo        .start ))  Rotation(0, 30) else
    if (in(Leo        .start , Virgo      .middle))  Rotation(0, 15) else
    if (in(Virgo      .middle, Libra      .middle))  Rotation(0)     else
    if (in(Libra      .middle, Sagittarius.start )) -Rotation(0, 15) else
    if (in(Sagittarius.start , Aquarius   .start )) -Rotation(0, 30) else
    if (in(Aquarius   .start , Pisces     .middle)) -Rotation(0, 15) else
      throw new IllegalArgumentException
  }
}
