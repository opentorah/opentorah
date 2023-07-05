package org.opentorah.astronomy

import Zodiac.*
import org.opentorah.astronomy.Angles.{Position, Rotation}

// KH 17:5-6
object MoonLongitudeSightingAdjustment extends MapTable.ZodiacRotationMapTable(
  Aries       -> "0°59′",
  Taurus      -> "1°0 ′",
  Gemini      -> "0°58′",
  Cancer      -> "0°52′",
  Leo         -> "0°43′",
  Virgo       -> "0°37′",
  Libra       -> "0°34′",
  Scorpio     -> "0°34′",
  Sagittarius -> "0°36′",
  Capricorn   -> "0°44′",
  Aquarius    -> "0°53′",
  Pisces      -> "0°58′"
)
