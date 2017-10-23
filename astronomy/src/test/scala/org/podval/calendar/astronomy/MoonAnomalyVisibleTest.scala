package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem.Angle

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MoonAnomalyVisibleTest extends FlatSpec {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(MoonAnomalyVisible.misprinted)
  }

  it should "be correct" in {
    test(MoonAnomalyVisible.table)
  }

  private def test(table: InterpolatedTable): Unit = {
    for (maslul <- (0 to 18).map(_ * 10).map(Angle(_))) {
      val mnas = table.calculate(maslul).abs
      val e: Double = MoonAnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = MoonAnomalyVisible.mnasfrome(maslul, e)
      val mnasRound = mnas.roundToMinutes
      assertResult(mnas.roundToMinutes)(mnasfrome.roundToMinutes)
    }
  }
}
