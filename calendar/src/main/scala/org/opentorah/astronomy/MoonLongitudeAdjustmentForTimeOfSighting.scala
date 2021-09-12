package org.opentorah.astronomy

import org.opentorah.angles.Angles.{Position, Rotation}

// KH 14:5
object MoonLongitudeAdjustmentForTimeOfSighting:
  final def calculate(sun: Position): Rotation =
    import Zodiac.*
    def in(from: Position, to: Position): Boolean = (from <= sun) && (sun < to)

    if in(Pisces     .middle, Aries      .middle) then  Rotation(0)     else
    if in(Aries      .middle, Gemini     .start ) then  Rotation(0, 15) else
    if in(Gemini     .start , Leo        .start ) then  Rotation(0, 30) else
    if in(Leo        .start , Virgo      .middle) then  Rotation(0, 15) else
    if in(Virgo      .middle, Libra      .middle) then  Rotation(0)     else
    if in(Libra      .middle, Sagittarius.start ) then -Rotation(0, 15) else
    if in(Sagittarius.start , Aquarius   .start ) then -Rotation(0, 30) else
    if in(Aquarius   .start , Pisces     .middle) then -Rotation(0, 15) else
      throw IllegalArgumentException()
