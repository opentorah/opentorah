package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem.Angle

class MoonAnomalyMeanTest extends FlatSpec {
  behavior of "Mean Moon Anomaly"

  it should "calculate for 29 days in two steps" in {
    // TODO How did Rambam calculate value for 29 days?
//    assertResult(Angle(18, 53, 4))(MoonAnomalyMean.table.month)
    assertResult(Angle(18, 53, 6))(MoonAnomalyMean.table.ten*3-MoonAnomalyMean.table.one)
    assertResult(Angle(18, 53, 6))(MoonAnomalyMean.table.ten*2+MoonAnomalyMean.table.one*9)
  }
}
