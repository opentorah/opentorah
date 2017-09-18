package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.jewish
import jewish.Jewish
import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SunLongitudeMeanTest extends FlatSpec {

  behavior of "Mean Longitude"

  it should "round to the same as Almagest" in {
    assertResult(SunLongitudeMean.rambamValue.roundToSeconds)(SunLongitudeMean.almagestValue.roundToSeconds)
  }

  it should "calculate for 29 days in two steps" in {
    assertResult(SunLongitudeMean.table.month)(SunLongitudeMean.table.ten*3-SunLongitudeMean.table.one)
  }

  it should "calculate correctly for the regular year" ignore {
    assertResult(SunLongitudeMean.table.year)(SunLongitudeMean.table.one*354)
    assertResult(SunLongitudeMean.table.year)(SunLongitudeMean.fromTable(354))
  }

  it should "make a full circle in a year" in {
    // TODO why isn't the NumberSystem type parameter inferred?
    assert(SunLongitudeMean.rambamValue *[Jewish] jewish.Sun.yearOfRavAda > AngleNumberSystem.period)
    assert(SunLongitudeMean.rambamValue *[Jewish] jewish.Sun.yearOfShmuel > AngleNumberSystem.period)
  }
}
