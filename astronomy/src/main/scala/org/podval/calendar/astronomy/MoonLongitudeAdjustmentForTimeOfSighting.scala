package org.podval.calendar.astronomy

import org.podval.calendar.angle.Angles.{Rotation, Position}

// KH 14:5
object MoonLongitudeAdjustmentForTimeOfSighting {
  // TODO see notes in Moznaim Rambam
  // TODO sun longitude: mean or true?
  final def calculate(sun: Position): Rotation = {
    import Zodiac._
    def in(from: Position, to: Position): Boolean = (from <= sun) && (sun < to)

    if (in(Pisces     .middle, Aries      .middle))  Rotation(0)     else
    if (in(Aries      .middle, Gemini     .start ))  Rotation(0, 15) else
    // TODO 15 in most editions!
    if (in(Gemini     .start , Leo        .start ))  Rotation(0, 30) else
    if (in(Leo        .start , Virgo      .middle))  Rotation(0, 15) else
    if (in(Virgo      .middle, Libra      .middle))  Rotation(0)     else
    if (in(Libra      .middle, Sagittarius.start )) -Rotation(0, 15) else
    // TODO 15 in most editions!
    if (in(Sagittarius.start , Aquarius   .start )) -Rotation(0, 30) else
    if (in(Aquarius   .start , Pisces     .middle)) -Rotation(0, 15) else
      throw new IllegalArgumentException
  }
}
