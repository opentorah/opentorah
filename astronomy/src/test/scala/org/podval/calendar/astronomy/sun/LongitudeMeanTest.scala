package org.podval.calendar.astronomy.sun
import org.podval.calendar.dates.Sun
import org.podval.calendar.dates.jewish.Jewish
import org.scalatest.FlatSpec


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
class LongitudeMeanTest extends FlatSpec {

  behavior of "Mean Longitude"

  it should "calculate to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days == 1) ||
      (LongitudeMean.calculated(days) < LongitudeMean.value(days)))}
  }


  it should "be what Almagest rounds to" in {
    assertResult(LongitudeMean.rambamValue)(LongitudeMean.almagestValue.roundToSeconds)
  }


  it should "round from Almagest to less than printed" in {
    LongitudeMean.keys.foreach{(days: Int) => assert((days <= 10) ||
      (LongitudeMean.almagest(days).roundToSeconds < LongitudeMean.value(days)))}
  }


  it should "calculate for 29 days in two steps" in {
    assertResult(LongitudeMean.value(29))(LongitudeMean.value(10)*3-LongitudeMean.value(1))
  }


  it should "calculate correctly for the regular year" ignore {
    assertResult(LongitudeMean.value(354))(LongitudeMean.value(100)*3+LongitudeMean.value(10)*5+LongitudeMean.value(1)*4)
  }


  it should "make a full circle in a year" in {
    // TODO why isn't the NumberSystem type parameter inferred?
    println(LongitudeMean.exact_ *[Jewish] Sun.yearOfRavAda)
    println(LongitudeMean.exact_ *[Jewish] Sun.yearOfShmuel)
  }
}
