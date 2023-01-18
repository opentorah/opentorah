package org.opentorah.astronomy

import Zodiac.*
import org.opentorah.astronomy.Angles.{Position, Rotation}

// KH 17:8-9
object MoonLatitudeSightingAdjustment extends MapTable[Zodiac, Rotation](
  Aries       -> "0° 9′",
  Taurus      -> "0°10′",
  Gemini      -> "0°16′",
  Cancer      -> "0°27′",
  Leo         -> "0°38′",
  Virgo       -> "0°44′",
  Libra       -> "0°46′",
  Scorpio     -> "0°45′",
  Sagittarius -> "0°44′",
  Capricorn   -> "0°36′",
  Aquarius    -> "0°27′",
  Pisces      -> "0°12′"
)(Rotation(_)):
  
  def calculate(moonLongitudeTrue: Position): Rotation = value(Zodiac.forPosition(moonLongitudeTrue))
