package org.podval.calendar.astronomy

import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.jewish
import jewish.Jewish
import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class SunLongitudeMeanTest extends FlatSpec {

  behavior of "Mean Longitude"

  it should "calculate to less than printed" in {
    SunLongitudeMean.keys.foreach{(days: Int) => assert((days == 1) ||
      (SunLongitudeMean.calculated(days).normal < SunLongitudeMean.value(days)))}
  }

  it should "be what Almagest rounds to" in {
    assertResult(SunLongitudeMean.rambamValue)(SunLongitudeMean.almagestValue.roundToSeconds)
  }

  it should "round from Almagest to less than printed" in {
    SunLongitudeMean.keys.foreach{(days: Int) => assert((days <= 10) ||
      (SunLongitudeMean.almagest(days).roundToSeconds.normal < SunLongitudeMean.value(days)))}
  }

  it should "calculate for 29 days in two steps" in {
    assertResult(SunLongitudeMean.value(29))(SunLongitudeMean.value(10)*3-SunLongitudeMean.value(1))
  }

  it should "calculate correctly for the regular year" ignore { // TODO test fails!
    // TODO what is the result of direct multiplication on 354?
    assertResult(SunLongitudeMean.value(354))(SunLongitudeMean.value(100)*3+SunLongitudeMean.value(10)*5+SunLongitudeMean.value(1)*4)
  }

  it should "make a full circle in a year" in {
    // TODO why isn't the NumberSystem type parameter inferred?
    assert(SunLongitudeMean.exact_ *[Jewish] jewish.Sun.yearOfRavAda > AngleNumberSystem.period)
    assert(SunLongitudeMean.exact_ *[Jewish] jewish.Sun.yearOfShmuel > AngleNumberSystem.period)
  }
}
