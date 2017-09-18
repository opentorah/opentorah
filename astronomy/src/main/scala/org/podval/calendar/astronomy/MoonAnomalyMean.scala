package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

object MoonAnomalyMean extends DayData {
  // TODO opposite direction!
  override val value: Map[Days, Angle] = Map(
    // KH 14:3
    1     -> Angle( 13,  3, 54),
    10    -> Angle(130, 39,  0),
    100   -> Angle(226, 29, 53),
    1000  -> Angle(104, 58, 50),
    10000 -> Angle(329, 48, 20),
    29    -> Angle( 18, 53,  4),
    // KH 14:4
    354   -> Angle(305,  0, 13)
  )

  val exact_ = Angle(13,3,53,55,49)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 3, 53, 53) -
  // as explanation of the value for 100 days (7 missing seconds)

  override val almagestValue = Angle(13,3,53,56,17,51,59)
}
