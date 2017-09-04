package org.podval.calendar.time

import org.scalatest.FlatSpec
import org.podval.calendar.numbers.BigRational
import SimpleTimeNumberSystem.Interval

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class TimeNumberSystemTest extends FlatSpec {

  "toRational()" should "be correct" in {
    assertResult(BigRational(-3, 1))(Interval(-3).toRational)
    assertResult(BigRational(3, 1))(Interval(3).toRational)
    assertResult(BigRational(3*24+5, 1*24))(Interval(3, 5).toRational)
    assertResult(-BigRational(3*24+5, 1*24))(Interval(-3, 5).toRational)
    assertResult(BigRational((3*24+5)*1080+4, 1*24*1080))(Interval(3, 5, 4).toRational)
    assertResult(BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76))(Interval(3, 5, 4, 1).toRational)
    assertResult(-BigRational((3*24+5)*1080+4, 1*24*1080))(Interval(-3, 5, 4).toRational)
    assertResult(BigRational(-((3*24+5)*1080+4), 1*24*1080))(Interval(-3, 5, 4).toRational)
  }

  "fromRational()" should "be correct" in {
    def test(value: Interval): Unit =
      assertResult(value)(Interval.fromRational(value.toRational))

    test(Interval(3))
    test(Interval(3, 5))
    test(Interval(3, 5, 4))
    test(Interval(-3, 5, 4))
    test(Interval(3, 5, 4, 1))
    test(Interval(-3, 5, 4, 1))
  }

  "roundTo()" should "be correct" in {
    assertResult(Interval(3))(Interval(3).roundTo(0))
    assertResult(Interval(3))(Interval(3).roundTo(1))
    assertResult(Interval(3))(Interval(3, 5).roundTo(0))
    assertResult(Interval(3, 5))(Interval(3, 5).roundTo(1))
    assertResult(Interval(3, 5))(Interval(3, 5).roundTo(2))
    assertResult(Interval(3))(Interval(3, 5, 4).roundTo(0))
    assertResult(Interval(3, 5))(Interval(3, 5, 4).roundTo(1))
    assertResult(Interval(3, 5, 4))(Interval(3, 5, 4).roundTo(2))
    assertResult(Interval(3, 5, 4))(Interval(3, 5, 4).roundTo(3))
    assertResult(Interval(-3))(Interval(-3, 5, 4).roundTo(0))
    assertResult(Interval(-3, 5))(Interval(-3, 5, 4).roundTo(1))
    assertResult(Interval(-3, 5, 4))(Interval(-3, 5, 4).roundTo(2))
    assertResult(Interval(-3, 5, 4))(Interval(-3, 5, 4).roundTo(3))
    assertResult(Interval(3, 5, 4))(Interval(3, 5, 4, 1).roundTo(2))
    assertResult(Interval(-3, 5, 4))(Interval(-3, 5, 4, 1).roundTo(2))
    assertResult(Interval(-3, 5, 4))(Interval(-3, 5, 4, 37).roundTo(2))
    assertResult(Interval(-3, 5, 5))(Interval(-3, 5, 4, 38).roundTo(2))
    assertResult(Interval(-3, 5, 5))(Interval(-3, 5, 4, 39).roundTo(2))
  }

  "toString()" should "be correct" in {
    assertResult("3d")(Interval(3).toString)
    assertResult("3d5h")(Interval(3, 5).toString)
    assertResult("3d5h0p")(Interval(3, 5).toString(2))
    assertResult("3d5h0p0m")(Interval(3, 5).toString(3))
    assertResult("3d5h0p0m0")(Interval(3, 5).toString(4))
    assertResult("3d5h4p")(Interval(3, 5, 4).toString)
    assertResult("-3d5h4p")(Interval(-3, 5, 4).toString)
    assertResult("3d5h4p1m")(Interval(3, 5, 4, 1).toString)
    assertResult("-3d5h4p1m")(Interval(-3, 5, 4, 1).toString)
  }
}
