package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles.{Rotation, Position}

object MoonLongitudeSightingAdjustment {

  // KH 17:5-6
  final def calculate(moonLongitudeTrue: Position): Rotation = {
    import Zodiac._
    inZodiac(moonLongitudeTrue) match {
      case Aries       => Rotation(0, 59)
      case Taurus      => Rotation(1)
      case Gemini      => Rotation(0, 58)
      case Cancer      => Rotation(0, 52)
      case Leo         => Rotation(0, 43)
      case Virgo       => Rotation(0, 37)
      case Libra       => Rotation(0, 34)
      case Scorpio     => Rotation(0, 34)
      case Sagittarius => Rotation(0, 36)
      case Capricorn   => Rotation(0, 44)
      case Aquarius    => Rotation(0, 53)
      case Pisces      => Rotation(0, 58)
    }
  }
}
