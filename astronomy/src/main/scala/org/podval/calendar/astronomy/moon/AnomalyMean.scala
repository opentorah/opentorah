package org.podval.calendar.astronomy.moon

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.DayData


object AnomalyMean extends DayData {
  override val value: Map[Days, Angle] = Map(
    1     -> Angle(13,3,54),
    10    -> Angle(130, 39, 0),
    100   -> Angle(226, 29, 53),
    1000  -> Angle(104, 58, 50),
    10000 -> Angle(329, 48, 20),
    29    -> Angle(18, 53, 4),
    354   -> Angle(305, 0, 13)
  )

  val exact_ = Angle(13,3,53,55,49)

  override val almagestValue = Angle(13,3,53,56,17,51,59)
}
