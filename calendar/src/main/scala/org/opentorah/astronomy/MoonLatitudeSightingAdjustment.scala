package org.opentorah.astronomy

import Angles.{Position, Rotation}

object MoonLatitudeSightingAdjustment:

  // KH 17:8-9
  final def calculate(moonLongitudeTrue: Position): Rotation =
    import Zodiac.*
    inZodiac(moonLongitudeTrue) match
      case Aries       => Rotation(0,  9)
      case Taurus      => Rotation(0, 10)
      case Gemini      => Rotation(0, 16)
      case Cancer      => Rotation(0, 27)
      case Leo         => Rotation(0, 38)
      case Virgo       => Rotation(0, 44)
      case Libra       => Rotation(0, 46)
      case Scorpio     => Rotation(0, 45)
      case Sagittarius => Rotation(0, 44)
      case Capricorn   => Rotation(0, 36)
      case Aquarius    => Rotation(0, 27)
      case Pisces      => Rotation(0, 12)
