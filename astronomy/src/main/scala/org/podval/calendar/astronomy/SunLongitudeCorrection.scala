package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

// TODO there are some diagrams in the Moznaim Rambam at this point.
object SunLongitudeCorrection extends Angle2Angle {
  final override val table: Table = new Table {
    // KH 13:2-3
    override def calculate(sunCourse: Angle): Angle = {
      val angle: Angle = sunCourse.canonical.roundToDegrees // KH 13:9
      if (angle < Angle(180)) -interpolate(angle) else
      if (angle > Angle(180))  interpolate(Angle(360) - angle) else
        Angle(0)
    }

    // KH 13:4
    override val a10 : Angle = Angle(0, 20)
    override val a20 : Angle = Angle(0, 40)
    override val a30 : Angle = Angle(0, 58)
    override val a40 : Angle = Angle(1, 15)
    override val a50 : Angle = Angle(1, 29)
    override def a60 : Angle = Angle(1, 41)
    override val a70 : Angle = Angle(1, 51)
    override val a80 : Angle = Angle(1, 57)
    override val a90 : Angle = Angle(1, 59)
    override val a100: Angle = Angle(1, 58)
    override val a110: Angle = Angle(1, 53)
    override val a120: Angle = Angle(1, 45)
    override val a130: Angle = Angle(1, 33)
    override val a140: Angle = Angle(1, 19)
    override val a150: Angle = Angle(1,  1)
    override val a160: Angle = Angle(0, 42)
    override val a170: Angle = Angle(0, 21)
  }
}
