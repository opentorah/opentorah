package org.podval.calendar.astronomy.moon

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.DayData


object LongitudeMean extends DayData {

  override val value: Map[Days, Angle] = Map(
    1     -> Angle( 13, 10, 35),
    10    -> Angle(131, 45, 50),
    100   -> Angle(237, 38, 23),
    1000  -> Angle(216, 23, 50),
    10000 -> Angle(  3, 58, 20),
    29    -> Angle( 22,  6, 56),
    354   -> Angle(344, 26, 43)
  )

  //    val exactInDegrees = 13.176397222222223
  val exact_ = Angle(13,10,35,1,48,1)

  override val almagestValue = Angle(13,10,34,58,33,30,30)
}
