package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem.Angle
import Angle2Angle.Table

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MoonAnomalyVisibleTest extends FlatSpec {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(MoonAnomalyVisible.misprinted)
  }

  it should "be correct" in {
    test(MoonAnomalyVisible.table)
  }

  private def test(table: Table): Unit = {
    for (maslul <- Angle2Angle.keys) {
      val mnas = table.calculate(maslul).abs
      val e: Double = MoonAnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = MoonAnomalyVisible.mnasfrome(maslul, e)
      val mnasRound = mnas.roundToMinutes
      assertResult(mnas.roundToMinutes)(mnasfrome.roundToMinutes)
    }
  }
}
