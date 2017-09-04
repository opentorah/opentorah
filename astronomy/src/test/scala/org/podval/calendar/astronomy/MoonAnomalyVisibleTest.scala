package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem.Angle
import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MoonAnomalyVisibleTest extends FlatSpec {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(MoonAnomalyVisible.MISPRINTED)
  }

  it should "be correct" in {
    test(MoonAnomalyVisible.VALUES)
  }

  private def test(table: Map[Angle, Angle]): Unit = {
    for ((maslul, mnas) <- table) {
      val e: Double = MoonAnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = MoonAnomalyVisible.mnasfrome(maslul, e)
      val mnas_ = mnasfrome.roundToMinutes

      assert(mnas == mnas_)
    }
  }
}
