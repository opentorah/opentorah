package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, Position}

object MoonLatitudeSightingAdjustment {

  // KH 17:8-9
  final def calculate(moonLongitudeTrue: Position): Angle = {
    import Zodiac._
    inZodiac(moonLongitudeTrue) match {
      case Aries       => Angle(0,  9)
      case Taurus      => Angle(0, 10)
      case Gemini      => Angle(0, 16)
      case Cancer      => Angle(0, 27)
      case Leo         => Angle(0, 38)
      case Virgo       => Angle(0, 44)
      case Libra       => Angle(0, 46)
      case Scorpio     => Angle(0, 45)
      case Sagittarius => Angle(0, 44)
      case Capricorn   => Angle(0, 36)
      case Aquarius    => Angle(0, 27)
      case Pisces      => Angle(0, 12)
    }
  }
}
