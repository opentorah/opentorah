package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}

object SunApogee extends DayData {
  // KH 12:2
  override val value: Map[Days, Angle] = Map(
    10    -> Angle(0,  0,  1, 30),
    100   -> Angle(0,  0, 15),
    1000  -> Angle(0,  2, 30),
    10000 -> Angle(0, 25),
    29    -> Angle(0,  0,  4), // TODO: veod!
    354   -> Angle(0,  0, 53)
  )

  override val almagestValue = Angle(0) // TODO
}
