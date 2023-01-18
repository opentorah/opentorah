package org.opentorah.astronomy

import Angles.{Position, Rotation}
import Zodiac.*

// KH 14:5-6
// TODO if the table is symmetric, why isn't the text exploiting this like in the MoonLatitude?
object MoonLongitudeAdjustmentForTimeOfSighting extends OrderedTable[Position, Position, Rotation](
  Aries      .start  -> " 0°   ",
  Aries      .middle -> " 0°15′",
  Gemini     .start  -> " 0°30′",
  Leo        .start  -> " 0°15′",
  Virgo      .middle -> " 0°   ",
  Libra      .middle -> "-0°15′",
  Sagittarius.start  -> "-0°30′",
  Aquarius   .start  -> "-0°15′",
  Pisces     .middle -> " 0°   "
)(identity, Rotation(_)):

  def calculate(sun: Position): Rotation = find(sun)
