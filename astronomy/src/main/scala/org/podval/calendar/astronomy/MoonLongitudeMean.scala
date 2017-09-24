package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import Days2Angle.Table

object MoonLongitudeMean extends Days2Angle {
  final override val table = new Table {
    // KH 14:1
    final override val one        : Angle = Angle( 13, 10, 35)
    // KH 14:2
    final override val ten        : Angle = Angle(131, 45, 50)
    final override val hundred    : Angle = Angle(237, 38, 23)
    final override val thousand   : Angle = Angle(216, 23, 50)
    final override val tenThousand: Angle = Angle(  3, 58, 20)

    final override val month      : Angle = Angle( 22,  6, 56)
    final override val year       : Angle = Angle(344, 26, 43)
  }

  // KH 14:4
  final override val atEpoch: AnglePoint = Zodiac.Taurus.at(Angle(1, 14, 43))


  // TODO does this correspond to the lunar period?

  //    val exactInDegrees = 13.176397222222223
  final override val rambamValue = Angle(13, 10, 35, 1, 48, 1)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 10, 35, 3) -
  // as explanation of the value for 100 days (3 extra seconds)

  final override val almagestValue = Angle(13,10,34,58,33,30,30)
}
