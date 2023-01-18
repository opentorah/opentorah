package org.opentorah.astronomy

import Angles.Rotation
import Days2Rotation.Days

class MoonAnomalyMeanTest extends Days2RotationTest(MoonAnomalyMean):
  behavior of "Mean Moon Anomaly"

  it should "have printed values not exact" in {
    what.nonReconstructable.nonEmpty should be (true)
  }

  it should "calculate for 29 days in two steps" in {
//    MoonAnomalyMean.table.month shouldBe Rotation(18, 53, 4)
    (what.value(Days.Ten)*3-what.value(Days.One)).canonical shouldBe Rotation("18°53′6″")
    (what.value(Days.Ten)*2+what.value(Days.One)*9).canonical shouldBe Rotation("18°53′6″")
  }
