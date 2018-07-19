package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, Position}

object MoonLongitudeSightingAdjustment {

  // KH 17:5-6
  final def calculate(moonLongitudeTrue: Position): Angle = {
    import Zodiac._
    inZodiac(moonLongitudeTrue) match {
      case Aries       => Angle(0, 59)
      case Taurus      => Angle(1)
      case Gemini      => Angle(0, 58)
      case Cancer      => Angle(0, 52)
      case Leo         => Angle(0, 43)
      case Virgo       => Angle(0, 37)
      case Libra       => Angle(0, 34)
      case Scorpio     => Angle(0, 34)
      case Sagittarius => Angle(0, 36)
      case Capricorn   => Angle(0, 44)
      case Aquarius    => Angle(0, 53)
      case Pisces      => Angle(0, 58)
    }
  }
}
