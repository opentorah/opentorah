package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import Days2Angle.Table

object SunLongitudeMean extends Days2Angle {
  // KH 12:1
  final override val table = new Table {
    final override val one        : Angle = Angle(  0, 59,  8)
    final override val ten        : Angle = Angle(  9, 51, 23)
    final override val hundred    : Angle = Angle( 98, 33, 53)
    final override val thousand   : Angle = Angle(265, 38, 50) // remainder
    final override val tenThousand: Angle = Angle(136, 28, 20)

    // TODO reconstructs if calculated as 3*v(10)-v(1); not if using the exact value :)
    final override val month      : Angle = Angle( 28, 35,  1)
    final override val year       : Angle = Angle(348, 55, 15)  // TODO ??
  }

  // Moznaim Rambam in English gives this value in KH 12:1 note 1 (without a reference) as the one
  // Rambam uses in his calculations.
  // TODO which of the year lengths does this correspond?
  final override val rambamValue: Angle = Angle(0, 59, 8, 19, 48)

  final override val almagestValue = Angle(0, 59, 8, 17, 13, 12, 31)


  // TODO move into tests
  def main(args: Array[String]) {
    def m(n: Int) = (rambamValue * n).roundTo(2)
    for (n <- List(1, 10, 100, 1000, 10000, 354))
      println(n + " " + m(n))

    val v29 = Angle(9, 51, 23)*3 - Angle(0, 59, 8)
    println(v29)

    val v354 = Angle(98, 33, 53)*3 + Angle(9, 51, 23)*5 + Angle(0, 59, 8)*4
    println(v354)
  }
}
