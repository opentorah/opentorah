package org.podval.calendar.time

import org.scalatest.FlatSpec
import org.podval.calendar.numbers.BigRational
import SimpleTimeNumberSystem.{Vector, Point}

final class TimeNumberSystemTest extends FlatSpec {

  val s: SimpleTimeNumberSystem.type = SimpleTimeNumberSystem

  "isZero()/isPositive()/isNegative()/signum()/abs()/negate()" should "be correct" in {
    assertResult(true)(Vector(0).isZero)
    assertResult(false)(Vector(1).isZero)
    assertResult(false)(Vector(-1).isZero)
    assertResult(false)(Vector(0, 1).isZero)
    assertResult(false)(Vector(0, -1).isZero)

    assertResult(false)(Vector(0).isPositive)
    assertResult(true)(Vector(1).isPositive)
    assertResult(false)(Vector(-1).isPositive)
    assertResult(true)(Vector(0, 1).isPositive)
    assertResult(false)(Vector(0, -1).isPositive)

    assertResult(false)(Vector(0).isNegative)
    assertResult(false)(Vector(1).isNegative)
    assertResult(true)(Vector(-1).isNegative)
    assertResult(false)(Vector(0, 1).isNegative)
    assertResult(true)(Vector(0, -1).isNegative)

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
    assertResult(0)(Vector(0).head)
    assertResult(0)(Vector(0).tail(0))
    assertResult(0)(Vector(0).tail(1))
    assertResult(0)(Vector(0, 2).head)
    assertResult(2)(Vector(0, 2).tail(0))
    assertResult(0)(Vector(0, 2).tail(1))

    assertResult(Seq(3))(Vector(0).head(3).digits)
    assertResult(Seq(0, 2))(Vector(0).tail(0, 2).digits)
    assertResult(Seq(0, 0, 3))(Vector(0).tail(1, 3).digits)
    assertResult(Seq(4, 2))(Vector(0, 2).head(4).digits)
    assertResult(Seq(0, 3))(Vector(0, 2).tail(0, 3).digits)
    assertResult(Seq(0, 2, 3))(Vector(0, 2).tail(1, 3).digits)

    assertResult(0)(Vector(0).head(3).length)
    assertResult(1)(Vector(0).tail(0, 2).length)
    assertResult(2)(Vector(0).tail(1, 3).length)
    assertResult(1)(Vector(0, 2).head(4).length)
    assertResult(1)(Vector(0, 2).tail(0, 3).length)
    assertResult(2)(Vector(0, 2).tail(1, 3).length)
  }

  "add()" should "be correct" in {
    assertResult(Vector(0))(Vector(0) + Vector(0))
    assertResult(Vector(0))(Vector(0) - Vector(0))
    assertResult(Vector(0, 1))(Vector(0, 1) + Vector(0))
    assertResult(Vector(0, 1))(Vector(0, 1) - Vector(0))
    assertResult(Vector(0, 2))(Vector(0, 1) +  Vector(0, 1))
    assertResult(Vector(0, 0))(Vector(0, 1) - Vector(0, 1))
  }

  "normal()/canonical()" should "be correct" in {
    assertResult("1850152d")(Vector(1816909, 751829, 49683240).canonical.toString)
    assertResult("1850152d")(Vector(1816909, 751829+46003).normal.canonical.toString)
    assertResult("1850152d")(Vector(1816909+33243).normal.canonical.toString)
  }

  "toRational()" should "be correct" in {
    assertResult(BigRational(-3, 1))(Vector(-3).toRational)
    assertResult(BigRational(3, 1))(Vector(3).toRational)
    assertResult(BigRational(3*24+5, 1*24))(Vector(3, 5).toRational)
    assertResult(-BigRational(3*24+5, 1*24))(-Vector(3, 5).toRational)
    assertResult(-BigRational(3*24+5, 1*24))(Vector(-3, -5).toRational)
    assertResult(BigRational((3*24+5)*1080+4, 1*24*1080))(Vector(3, 5, 4).toRational)
    assertResult(BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76))(Vector(3, 5, 4, 1).toRational)
    assertResult(-BigRational((3*24+5)*1080+4, 1*24*1080))(-Vector(3, 5, 4).toRational)
    assertResult(BigRational(-((3*24+5)*1080+4), 1*24*1080))(-Vector(3, 5, 4).toRational)
  }

  "fromRational()" should "be correct" in {
    def test(value: Vector): Unit = {
      val rational = value.toRational
      val number = Vector.fromRational(rational)
      assertResult(value)(number)
    }

    test(-Vector(3, 5))
    test(Vector(3))
    test(-Vector(3))
    test(Vector(3, 5))
    test(Vector(3, 5, 4))
    test(-Vector(3, 5, 4))
    test(Vector(3, 5, 4, 1))
    test(-Vector(3, 5, 4, 1))
  }

  "roundTo()" should "be correct" in {
    assertResult(Vector(3))(Vector(3).roundTo(0))
    assertResult(Vector(3))(Vector(3).roundTo(1))
    assertResult(Vector(3))(Vector(3, 5).roundTo(0))
    assertResult(Vector(3, 5))(Vector(3, 5).roundTo(1))
    assertResult(Vector(3, 5))(Vector(3, 5).roundTo(2))
    assertResult(-Vector(3))((-Vector(3, 5)).roundTo(0))
    assertResult(-Vector(4))((-Vector(3, 12)).roundTo(0))
    assertResult(-Vector(3, 5))((-Vector(3, 5)).roundTo(1))
    assertResult(-Vector(3, 5))((-Vector(3, 5)).roundTo(2))
    assertResult(Vector(3))(Vector(3, 5, 4).roundTo(0))
    assertResult(Vector(3, 5))(Vector(3, 5, 4).roundTo(1))
    assertResult(Vector(3, 5, 4))(Vector(3, 5, 4).roundTo(2))
    assertResult(Vector(3, 5, 4))(Vector(3, 5, 4).roundTo(3))
    assertResult(Vector(-3))(Vector(-3, 5, 4).roundTo(0))
    assertResult(Vector(-3, 5))(Vector(-3, 5, 4).roundTo(1))
    assertResult(Vector(-3, 5, 4))(Vector(-3, 5, 4).roundTo(2))
    assertResult(Vector(-3, 5, 4))(Vector(-3, 5, 4).roundTo(3))
    assertResult(Vector(3, 5, 4))(Vector(3, 5, 4, 1).roundTo(2))
    assertResult(Vector(-3, 5, 4))(Vector(-3, 5, 4, 1).roundTo(2))
    assertResult(Vector(-3, 5, 4))(Vector(-3, 5, 4, 37).roundTo(2))
    assertResult(Vector(-3, 5, 5))(Vector(-3, 5, 4, 38).roundTo(2))
    assertResult(Vector(-3, 5, 5))(Vector(-3, 5, 4, 39).roundTo(2))
  }

  "toString()" should "be correct" in {
    assertResult("3d")(Vector(3).toString)
    assertResult("3d5h")(Vector(3, 5).toString)
    assertResult("3d5h0p")(Vector(3, 5).toString(2))
    assertResult("3d5h0p0m")(Vector(3, 5).toString(3))
    assertResult("3d5h0p0m0")(Vector(3, 5).toString(4))
    assertResult("3d5h4p")(Vector(3, 5, 4).toString)
    assertResult("-2d18h1076p")(Vector(-3, 5, 4).toString)
    assertResult("3d5h4p1m")(Vector(3, 5, 4, 1).toString)
    assertResult("-2d18h1075p75m")(Vector(-3, 5, 4, 1).toString)
    assertResult("-0d5h4p1m")((-Vector(0, 5, 4, 1)).toString)
  }
}
