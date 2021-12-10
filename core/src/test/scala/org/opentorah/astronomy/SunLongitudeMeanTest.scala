package org.opentorah.astronomy

import org.opentorah.calendar.jewish.Sun
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SunLongitudeMeanTest extends AnyFlatSpec, Matchers:

  behavior of "Mean Sun Longitude"

  it should "round to the same as Almagest" in {
    SunLongitudeMean.almagestValue.roundToSeconds shouldBe SunLongitudeMean.rambamValue.roundToSeconds
  }

  it should "calculate for 29 days in two steps" in {
    (SunLongitudeMean.ten*3-SunLongitudeMean.one) shouldBe SunLongitudeMean.month
    (SunLongitudeMean.ten*3-SunLongitudeMean.one) shouldBe Angles.Rotation(28, 35, 1)
    (SunLongitudeMean.ten*2+SunLongitudeMean.one*9) shouldBe Angles.Rotation(28, 34, 58)
  }

  it should "calculate correctly for the regular year" ignore {
    (SunLongitudeMean.one*354) shouldBe SunLongitudeMean.year
    SunLongitudeMean.calculate(354) shouldBe SunLongitudeMean.year
  }

  it should "make a full circle in a year" in {
    assert(SunLongitudeMean.rambamValue *(Sun.RavAda.yearLength.toRational, Angles.maxLength) > Angles.period)
    assert(SunLongitudeMean.rambamValue *(Sun.Shmuel.yearLength.toRational, Angles.maxLength) > Angles.period)
  }
