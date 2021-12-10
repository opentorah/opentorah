package org.opentorah.astronomy

import Angles.Rotation
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MoonAnomalyMeanTest extends AnyFlatSpec, Matchers:
  behavior of "Mean Moon Anomaly"

  it should "calculate for 29 days in two steps" in {
//    MoonAnomalyMean.table.month shouldBe Rotation(18, 53, 4)
    (MoonAnomalyMean.ten*3-MoonAnomalyMean.one).canonical shouldBe Rotation(18, 53, 6)
    (MoonAnomalyMean.ten*2+MoonAnomalyMean.one*9).canonical shouldBe Rotation(18, 53, 6)
  }
