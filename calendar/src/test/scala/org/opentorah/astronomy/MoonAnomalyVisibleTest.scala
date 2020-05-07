package org.opentorah.astronomy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.opentorah.angles.Angles.Position

class MoonAnomalyVisibleTest extends AnyFlatSpec with Matchers {

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in {
    test(MoonAnomalyVisible.misprinted)
  }

  it should "be correct" in {
    test(MoonAnomalyVisible.table)
  }

  private def test(table: InterpolatedTable[Position]): Unit = {
    for (maslul <- (0 to 18).map(_ * 10).map(Position(_))) {
      val mnas = table.calculate(maslul).abs
      val e: Double = MoonAnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = MoonAnomalyVisible.mnasfrome(maslul, e)
      val mnasRound = mnas.roundToMinutes
      mnasfrome.roundToMinutes shouldBe mnas.roundToMinutes
    }
  }
}
