package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle

object MoonLongitudeMean extends DayData {
  override val value: Map[Days, Angle] = Map(
    // KH 14:1
    1     -> Angle( 13, 10, 35),
    // KH 14:2
    10    -> Angle(131, 45, 50),
    100   -> Angle(237, 38, 23),
    1000  -> Angle(216, 23, 50),
    10000 -> Angle(  3, 58, 20),
    29    -> Angle( 22,  6, 56),
    354   -> Angle(344, 26, 43)
  )

  // TODO does this correspond to the lunar period?

  //    val exactInDegrees = 13.176397222222223
  val exact_ = Angle(13,10,35,1,48,1)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 10, 35, 3) -
  // as explanation of the value for 100 days (3 extra seconds)

  override val almagestValue = Angle(13,10,34,58,33,30,30)
}
