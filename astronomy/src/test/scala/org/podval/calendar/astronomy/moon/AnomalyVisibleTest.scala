package org.podval.calendar.astronomy.moon

import org.scalatest.FlatSpec

import org.podval.calendar.astronomy.angle.AngleNumberSystem.Angle


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class AnomalyVisibleTest extends FlatSpec {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(AnomalyVisible.MISPRINTED)
  }

  it should "be correct" in {
    test(AnomalyVisible.VALUES)
  }

  private def test(table: Map[Angle, Angle]): Unit = {
    for ((maslul, mnas) <- table) {
      val e: Double = AnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = AnomalyVisible.mnasfrome(maslul, e)
      val mnas_ = mnasfrome.roundToMinutes

      assert(mnas == mnas_)
    }
  }
}
