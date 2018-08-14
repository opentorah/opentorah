package org.podval.calendar.astronomy

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.angles.Angles.Rotation

class MoonAnomalyMeanTest extends FlatSpec with Matchers {
  behavior of "Mean Moon Anomaly"

  it should "calculate for 29 days in two steps" in {
    // TODO How did Rambam calculate value for 29 days?
//    MoonAnomalyMean.table.month shouldBe Rotation(18, 53, 4)
    MoonAnomalyMean.ten*3-MoonAnomalyMean.one shouldBe Rotation(18, 53, 6)
    MoonAnomalyMean.ten*2+MoonAnomalyMean.one*9 shouldBe Rotation(18, 53, 6)
  }
}
