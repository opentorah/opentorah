package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

object MoonLatitude extends Angle2Angle {
  final override val table: Table = new Table {
    // KH 16:13-15
    override def calculate(moonLatitudeCourse: Angle): Angle = {
      val angle: Angle = moonLatitudeCourse.canonical.roundToDegrees
      if (angle < Angle(180)) interpolate(angle) else
      if (angle > Angle(180)) interpolate(angle - Angle(180)) else
        Angle(0)
    }

    // KH 16:11
    override val a10 : Angle = Angle(0, 52)
    override val a20 : Angle = Angle(1, 43)
    override val a30 : Angle = Angle(2, 30)
    override val a40 : Angle = Angle(3, 13)
    override val a50 : Angle = Angle(3, 50)
    override def a60 : Angle = Angle(4, 20)
    override val a70 : Angle = Angle(4, 42)
    override val a80 : Angle = Angle(4, 55)
    override val a90 : Angle = Angle(5,  0)
    // KH 16:13
    override val a100: Angle = a80
    override val a110: Angle = a70
    override val a120: Angle = a60
    override val a130: Angle = a50
    override val a140: Angle = a40
    override val a150: Angle = a30
    override val a160: Angle = a20
    override val a170: Angle = a10
  }
}
