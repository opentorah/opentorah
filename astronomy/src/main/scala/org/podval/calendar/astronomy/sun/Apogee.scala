package org.podval.calendar.astronomy.sun

import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.angle.Angle
import org.podval.calendar.astronomy.DayData


object Apogee extends DayData {

  override val value = Map[Days, Angle](
    10    -> Angle(0,  0,  1, 30),
    100   -> Angle(0,  0, 15),
    1000  -> Angle(0,  2, 30),
    10000 -> Angle(0, 25),
    29    -> Angle(0,  0,  4), // TODO: veod!
    354   -> Angle(0,  0, 53)
  )

  override val almagestValue = Angle(0) // TODO
}
