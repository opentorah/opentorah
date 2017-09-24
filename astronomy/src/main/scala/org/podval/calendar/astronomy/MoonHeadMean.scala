package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.{Angle, AnglePoint}
import Days2Angle.Table

// TODO opposite direction!
object MoonHeadMean extends Days2Angle {

  final override val table = new Table {
    // KH 16:2
    final override val one        : Angle = Angle(  0,  3, 11) // few thirds less (f6)
    final override val ten        : Angle = Angle(  0, 31, 47)
    final override val hundred    : Angle = Angle(  5, 17, 43)
    final override val thousand   : Angle = Angle( 52, 57, 10)
    final override val tenThousand: Angle = Angle(169, 31, 40)

    final override val month      : Angle = Angle(  1, 32,  9)
    final override val year       : Angle = Angle( 18, 44, 42)
  }

  // KH 14:4; f7
  final override val atEpoch: AnglePoint = AnglePoint(180, 57, 28)


  final override val rambamValue = Angle(0) // TODO

  final override val almagestValue = Angle(0) // TODO
}
