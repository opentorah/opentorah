package org.podval.calendar.astronomy

import org.scalatest.{FlatSpec, Matchers}
import org.podval.calendar.angles.Angles.{Rotation, period}
import org.podval.calendar.jewish

class SunLongitudeMeanTest extends FlatSpec with Matchers {

  behavior of "Mean Sun Longitude"

  it should "round to the same as Almagest" in {
    SunLongitudeMean.almagestValue.roundToSeconds shouldBe SunLongitudeMean.rambamValue.roundToSeconds
  }

  it should "calculate for 29 days in two steps" in {
    (SunLongitudeMean.ten*3-SunLongitudeMean.one) shouldBe SunLongitudeMean.month
    (SunLongitudeMean.ten*3-SunLongitudeMean.one) shouldBe Rotation(28, 35, 1)
    (SunLongitudeMean.ten*2+SunLongitudeMean.one*9) shouldBe Rotation(28, 34, 58)
  }

  it should "calculate correctly for the regular year" ignore {
    (SunLongitudeMean.one*354) shouldBe SunLongitudeMean.year
    SunLongitudeMean.calculate(354) shouldBe SunLongitudeMean.year
  }

  it should "make a full circle in a year" in {
    assert(SunLongitudeMean.rambamValue.*[jewish.Jewish](jewish.Sun.yearOfRavAda) > period)
    assert(SunLongitudeMean.rambamValue.*[jewish.Jewish](jewish.Sun.yearOfShmuel) > period)
  }
}
