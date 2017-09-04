package org.podval.calendar.astronomy.sun

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.podval.calendar.astronomy.DayData

object LongitudeMean extends DayData {
  override val value: Map[Days, Angle] = Map(
    1     -> Angle(  0, 59,  8),
    10    -> Angle(  9, 51, 23),
    100   -> Angle( 98, 33, 53),
    1000  -> Angle(265, 38, 50), // remainder
    10000 -> Angle(136, 28, 20),
    29    -> Angle( 28, 35,  1), // reconstructs if calculated as 3*v(10)-v(1); not if using the exact value :)
    354   -> Angle(348, 55, 15)  // ??
  )


  val exact_ = Angle(0, 59, 8, 19, 48)  // TODO why was it 49?

  override val almagestValue = Angle(0, 59, 8, 17, 13, 12, 31)



  def main(args: Array[String]) {
    def m(n: Int) = (exact_ * n).roundTo(2)
    for (n <- List(1, 10, 100, 1000, 10000, 354))
      println(n + " " + m(n))

    val v29 = Angle(9, 51, 23)*3 - Angle(0, 59, 8)
    println(v29)

    val v354 = Angle(98, 33, 53)*3 + Angle(9, 51, 23)*5 + Angle(0, 59, 8)*4
    println(v354)
  }
}
