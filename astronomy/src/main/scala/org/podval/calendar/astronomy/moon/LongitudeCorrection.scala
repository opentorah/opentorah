package org.podval.calendar.astronomy.moon

import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle


object LongitudeCorrection {

  val VALUES: List[(String, Angle, Angle)] = List(
    ("middle of Taurus"        , Angle( 15), Angle(0)),
    ("beginning of Gemini"     , Angle( 30), Angle(0, 15)),
    ("beginning of Leo"        , Angle( 90), Angle(0, 15)),
    ("middle of Virgo"         , Angle(135), Angle(0, 15)),
    ("middle of Libra"         , Angle(165), Angle(0)),
    ("beginning of Sagittarius", Angle(210), Angle(0, -15)),
    ("beginning of Aquarius"   , Angle(270), Angle(0, -30)),
    ("middle of Pisces"        , Angle(315), Angle(0, -15))
  )
}
