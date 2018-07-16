package org.podval.calendar.time

import org.scalatest.FlatSpec
import org.podval.calendar.numbers.BigRational
import SimpleTimeNumberSystem.{Interval, Point}

final class TimeNumberSystemTest extends FlatSpec {

  val s: SimpleTimeNumberSystem.type = SimpleTimeNumberSystem

  "isZero()/isPositive()/isNegative()/signum()/abs()/negate()" should "be correct" in {
    assertResult(true)(Interval(0).isZero)
    assertResult(false)(Interval(1).isZero)
    assertResult(false)(Interval(-1).isZero)
    assertResult(false)(Interval(0, 1).isZero)
    assertResult(false)(Interval(0, -1).isZero)

    assertResult(false)(Interval(0).isPositive)
    assertResult(true)(Interval(1).isPositive)
    assertResult(false)(Interval(-1).isPositive)
    assertResult(true)(Interval(0, 1).isPositive)
    assertResult(false)(Interval(0, -1).isPositive)

    assertResult(false)(Interval(0).isNegative)
    assertResult(false)(Interval(1).isNegative)
    assertResult(true)(Interval(-1).isNegative)
    assertResult(false)(Interval(0, 1).isNegative)
    assertResult(true)(Interval(0, -1).isNegative)

    assertResult(0)(Point(0).signum)
    assertResult(1)(Point(1).signum)
    assertResult(-1)(Point(-1).signum)
    assertResult(1)(Point(0, 1).signum)
    assertResult(-1)(Point(0, -1).signum)

    assertResult(Point(0))(Point(0).abs)
    assertResult(Point(1))(Point(1).abs)
    assertResult(Point(1))(Point(-1).abs)
    assertResult(Point(0, 1))(Point(0, 1).abs)
    assertResult(Point(0, 1))(Point(0, -1).abs)

    assertResult(Point(0))(-Point(0))
    assertResult(Point(-1))(-Point(1))
    assertResult(Point(1))(-Point(-1))
    assertResult(Point(0, -1))(-Point(0, 1))
    assertResult(Point(0, 1))(-Point(0, -1))
  }

  "head()/tail()/length()" should "be correct" in {
    assertResult(0)(Interval(0).head)
    assertResult(0)(Interval(0).tail(0))
    assertResult(0)(Interval(0).tail(1))
    assertResult(0)(Interval(0, 2).head)
    assertResult(2)(Interval(0, 2).tail(0))
    assertResult(0)(Interval(0, 2).tail(1))

    assertResult(Seq(3))(Interval(0).head(3).digits)
    assertResult(Seq(0, 2))(Interval(0).tail(0, 2).digits)
    assertResult(Seq(0, 0, 3))(Interval(0).tail(1, 3).digits)
    assertResult(Seq(4, 2))(Interval(0, 2).head(4).digits)
    assertResult(Seq(0, 3))(Interval(0, 2).tail(0, 3).digits)
    assertResult(Seq(0, 2, 3))(Interval(0, 2).tail(1, 3).digits)

    assertResult(0)(Interval(0).head(3).length)
    assertResult(1)(Interval(0).tail(0, 2).length)
    assertResult(2)(Interval(0).tail(1, 3).length)
    assertResult(1)(Interval(0, 2).head(4).length)
    assertResult(1)(Interval(0, 2).tail(0, 3).length)
    assertResult(2)(Interval(0, 2).tail(1, 3).length)
  }

  "add()" should "be correct" in {
    assertResult(Interval(0))(Interval(0) + Interval(0))
    assertResult(Interval(0))(Interval(0) - Interval(0))
    assertResult(Interval(0, 1))(Interval(0, 1) + Interval(0))
    assertResult(Interval(0, 1))(Interval(0, 1) - Interval(0))
    assertResult(Interval(0, 2))(Interval(0, 1) +  Interval(0, 1))
    assertResult(Interval(0, 0))(Interval(0, 1) - Interval(0, 1))
  }

  "normal()/canonical()" should "be correct" in {
    assertResult("1850152d")(Interval(1816909, 751829, 49683240).canonical.toString)
    assertResult("1850152d")(Interval(1816909, 751829+46003).normal.canonical.toString)
    assertResult("1850152d")(Interval(1816909+33243).normal.canonical.toString)
  }

  "toRational()" should "be correct" in {
    assertResult(BigRational(-3, 1))(Interval(-3).toRational)
    assertResult(BigRational(3, 1))(Interval(3).toRational)
    assertResult(BigRational(3*24+5, 1*24))(Interval(3, 5).toRational)
    assertResult(-BigRational(3*24+5, 1*24))(-Interval(3, 5).toRational)
    assertResult(-BigRational(3*24+5, 1*24))(Interval(-3, -5).toRational)
    assertResult(BigRational((3*24+5)*1080+4, 1*24*1080))(Interval(3, 5, 4).toRational)
    assertResult(BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76))(Interval(3, 5, 4, 1).toRational)
    assertResult(-BigRational((3*24+5)*1080+4, 1*24*1080))(-Interval(3, 5, 4).toRational)
    assertResult(BigRational(-((3*24+5)*1080+4), 1*24*1080))(-Interval(3, 5, 4).toRational)
  }

  "fromRational()" should "be correct" in {
    def test(value: Interval): Unit = {
      val rational = value.toRational
      val number = Interval.fromRational(rational)
      assertResult(value)(number)
    }

    test(-Interval(3, 5))
    test(Interval(3))
    test(-Interval(3))
    test(Interval(3, 5))
    test(Interval(3, 5, 4))
    test(-Interval(3, 5, 4))
    test(Interval(3, 5, 4, 1))
    test(-Interval(3, 5, 4, 1))
  }

  "roundTo()" should "be correct" in {
    assertResult(Interval(3))(Interval(3).roundTo(0))
    assertResult(Interval(3))(Interval(3).roundTo(1))
    assertResult(Interval(3))(Interval(3, 5).roundTo(0))
    assertResult(Interval(3, 5))(Interval(3, 5).roundTo(1))
    assertResult(Interval(3, 5))(Interval(3, 5).roundTo(2))
    assertResult(-Interval(3))((-Interval(3, 5)).roundTo(0))
    assertResult(-Interval(4))((-Interval(3, 12)).roundTo(0))
    assertResult(-Interval(3, 5))((-Interval(3, 5)).roundTo(1))
    assertResult(-Interval(3, 5))((-Interval(3, 5)).roundTo(2))
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
    assertResult("-2d18h1076p")(Interval(-3, 5, 4).toString)
    assertResult("3d5h4p1m")(Interval(3, 5, 4, 1).toString)
    assertResult("-2d18h1075p75m")(Interval(-3, 5, 4, 1).toString)
    assertResult("-0d5h4p1m")((-Interval(0, 5, 4, 1)).toString)
  }
}
