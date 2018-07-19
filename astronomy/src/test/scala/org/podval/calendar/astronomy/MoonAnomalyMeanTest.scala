package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem.Rotation

class MoonAnomalyMeanTest extends FlatSpec {
  behavior of "Mean Moon Anomaly"

  it should "calculate for 29 days in two steps" in {
    // TODO How did Rambam calculate value for 29 days?
//    assertResult(Angle(18, 53, 4))(MoonAnomalyMean.table.month)
    assertResult(Rotation(18, 53, 6))(MoonAnomalyMean.ten*3-MoonAnomalyMean.one)
    assertResult(Rotation(18, 53, 6))(MoonAnomalyMean.ten*2+MoonAnomalyMean.one*9)
  }
}
