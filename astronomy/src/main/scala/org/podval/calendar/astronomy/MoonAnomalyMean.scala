package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import DayData.Table

object MoonAnomalyMean extends DayData {
  // TODO opposite direction!

  final override val table = new Table {
    // KH 14:3
    final override val one        : Angle = Angle( 13,  3, 54)
    final override val ten        : Angle = Angle(130, 39,  0)
    final override val hundred    : Angle = Angle(226, 29, 53)
    final override val thousand   : Angle = Angle(104, 58, 50)
    final override val tenThousand: Angle = Angle(329, 48, 20)

    final override val month      : Angle = Angle( 18, 53,  4)
    // KH 14:4
    final override val year       : Angle = Angle(305,  0, 13)
  }

  final override val rambamValue: Angle = Angle(13, 3, 53, 55, 49)

  // TODO Moznaim Rambam, KH 13:2f4: (13, 3, 53, 53) -
  // as explanation of the value for 100 days (7 missing seconds)

  final override val almagestValue = Angle(13, 3, 53, 56, 17, 51, 59)
}
