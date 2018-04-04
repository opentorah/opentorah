package org.podval.calendar.astronomy

import org.scalatest.FlatSpec
import org.podval.calendar.angle.AngleNumberSystem.{Angle, period}
import org.podval.calendar.jewish
import jewish.Jewish

class SunLongitudeMeanTest extends FlatSpec {

  behavior of "Mean Sun Longitude"

  it should "round to the same as Almagest" in {
    assertResult(SunLongitudeMean.rambamValue.roundToSeconds)(SunLongitudeMean.almagestValue.roundToSeconds)
  }

  it should "calculate for 29 days in two steps" in {
    // Rambam calculated value for 29 days as 3*10-1, not 2*10+9 :)
    assertResult(SunLongitudeMean.month)(SunLongitudeMean.ten*3-SunLongitudeMean.one)
    assertResult(Angle(28, 35, 1))(SunLongitudeMean.ten*3-SunLongitudeMean.one)
    assertResult(Angle(28, 34, 58))(SunLongitudeMean.ten*2+SunLongitudeMean.one*9)
  }

  it should "calculate correctly for the regular year" ignore {
    assertResult(SunLongitudeMean.year)(SunLongitudeMean.one*354)
    assertResult(SunLongitudeMean.year)(SunLongitudeMean.calculate(354))
  }

  it should "make a full circle in a year" in {
    // TODO why isn't the NumberSystem type parameter inferred?
    assert(SunLongitudeMean.rambamValue *[Jewish] jewish.Sun.yearOfRavAda > period)
    assert(SunLongitudeMean.rambamValue *[Jewish] jewish.Sun.yearOfShmuel > period)
  }
}
