package org.opentorah.astronomy

import Angles.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoonAnomalyVisibleTest extends AnyFlatSpec, Matchers:

  behavior of "Anomaly"

  it should "be correctly misprinted :)" in test(MoonAnomalyVisible.misprinted)

  it should "be correct" in test(MoonAnomalyVisible.corrected)

  private def test(table: OrderedRotationTable[Position]): Unit =
    for maslul <- (0 to 36).map(_ * 10).map(Position(_)) do // TODO does not work for interpolated values...
      val mnas = table.calculate(maslul).abs
      val e: Double = MoonAnomalyVisible.efrommnasround(maslul, mnas)
      val mnasfrome = MoonAnomalyVisible.mnasfrome(maslul, e)
      mnasfrome.roundToMinutes.abs shouldBe mnas.roundToMinutes // TODO eliminate abs()
