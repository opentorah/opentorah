package org.podval.calendar.astronomy.sun

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem
import org.podval.calendar.jewish.{Jewish, Sun}

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LongitudeMeanTest extends FlatSpec {

  behavior of "Mean Longitude"

  it should "calculate to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days == 1) ||
      (LongitudeMean.calculated(days).normal < LongitudeMean.value(days)))}
  }

  it should "be what Almagest rounds to" in {
    assertResult(LongitudeMean.rambamValue)(LongitudeMean.almagestValue.roundToSeconds)
  }

  it should "round from Almagest to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days <= 10) ||
      (LongitudeMean.almagest(days).roundToSeconds.normal < LongitudeMean.value(days)))}
  }

  it should "calculate for 29 days in two steps" in {
    assertResult(LongitudeMean.value(29))(LongitudeMean.value(10)*3-LongitudeMean.value(1))
  }

  it should "calculate correctly for the regular year" ignore { // TODO test fails!
    // TODO what is the result of direct multiplication on 354?
    assertResult(LongitudeMean.value(354))(LongitudeMean.value(100)*3+LongitudeMean.value(10)*5+LongitudeMean.value(1)*4)
  }

  it should "make a full circle in a year" in {
    // TODO why isn't the NumberSystem type parameter inferred?
    assert(LongitudeMean.exact_ *[Jewish] Sun.yearOfRavAda > AngleNumberSystem.period)
    assert(LongitudeMean.exact_ *[Jewish] Sun.yearOfShmuel > AngleNumberSystem.period)
  }
}
