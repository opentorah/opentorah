package org.podval.calendar.times

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.podval.calendar.numbers.BigRational
import SimpleTimes.{Point, Vector}

final class SimpleTimesTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  "isZero()/isPositive()/isNegative()/signum()/abs()/unary_-()" should "be correct" in {
    Vector(0).isZero shouldBe true
    Vector(1).isZero shouldBe false
    Vector(-1).isZero shouldBe false
    Vector(0, 1).isZero shouldBe false
    Vector(0, -1).isZero shouldBe false

    Vector(0).isPositive shouldBe false
    Vector(1).isPositive shouldBe true
    Vector(-1).isPositive shouldBe false
    Vector(0, 1).isPositive shouldBe true
    Vector(0, -1).isPositive shouldBe false

    Vector(0).isNegative shouldBe false
    Vector(1).isNegative shouldBe false
    Vector(-1).isNegative shouldBe true
    Vector(0, 1).isNegative shouldBe false
    Vector(0, -1).isNegative shouldBe true

    Point(0).signum shouldBe 0
    Point(1).signum shouldBe 1
    Point(-1).signum shouldBe-1
    Point(0, 1).signum shouldBe 1
    Point(0, -1).signum shouldBe -1

    Point(0).abs shouldBe Point(0)
    Point(1).abs shouldBe Point(1)
    Point(-1).abs shouldBe Point(1)
    Point(0, 1).abs shouldBe Point(0, 1)
    Point(0, -1).abs shouldBe Point(0, 1)

    -Point(0) shouldBe Point(0)
    -Point(1) shouldBe Point(-1)
    -Point(-1) shouldBe Point(1)
    -Point(0, 1) shouldBe Point(0, -1)
    -Point(0, -1) shouldBe Point(0, 1)
  }

  "head()/tail()/length()" should "be correct" in {
    Vector(0).head shouldBe 0
    Vector(0).tail(0) shouldBe 0
    Vector(0).tail(1) shouldBe 0
    Vector(0, 2).head shouldBe 0
    Vector(0, 2).tail(0) shouldBe 2
    Vector(0, 2).tail(1) shouldBe 0

    Vector(0).head(3).digits shouldBe Seq(3)
    Vector(0).tail(0, 2).digits shouldBe Seq(0, 2)
    Vector(0).tail(1, 3).digits shouldBe Seq(0, 0, 3)
    Vector(0, 2).head(4).digits shouldBe Seq(4, 2)
    Vector(0, 2).tail(0, 3).digits shouldBe Seq(0, 3)
    Vector(0, 2).tail(1, 3).digits shouldBe Seq(0, 2, 3)

    Vector(0).head(3).length shouldBe 0
    Vector(0).tail(0, 2).length shouldBe 1
    Vector(0).tail(1, 3).length shouldBe 2
    Vector(0, 2).head(4).length shouldBe 1
    Vector(0, 2).tail(0, 3).length shouldBe 1
    Vector(0, 2).tail(1, 3).length shouldBe 2
  }

  "+()" should "be correct" in {
    Vector(0) + Vector(0) shouldBe Vector(0)
    Vector(0) - Vector(0) shouldBe Vector(0)
    Vector(0, 1) + Vector(0) shouldBe Vector(0, 1)
    Vector(0, 1) - Vector(0) shouldBe Vector(0, 1)
    Vector(0, 1) +  Vector(0, 1) shouldBe Vector(0, 2)
    Vector(0, 1) - Vector(0, 1) shouldBe Vector(0, 0)
  }

  "normal()/canonical()" should "be correct" in {
    Vector(1816909, 751829, 49683240).canonical.toString shouldBe "1850152d"
    Vector(1816909, 751829+46003).normal.canonical.toString shouldBe "1850152d"
    Vector(1816909+33243).normal.canonical.toString shouldBe "1850152d"
  }

  "toRational()" should "be correct" in {
    Vector(-3).toRational shouldBe BigRational(-3, 1)
    Vector(3).toRational shouldBe BigRational(3, 1)
    Vector(3, 5).toRational shouldBe BigRational(3*24+5, 1*24)
    -Vector(3, 5).toRational shouldBe -BigRational(3*24+5, 1*24)
    Vector(-3, -5).toRational shouldBe -BigRational(3*24+5, 1*24)
    Vector(3, 5, 4).toRational shouldBe BigRational((3*24+5)*1080+4, 1*24*1080)
    Vector(3, 5, 4, 1).toRational shouldBe BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76)
    -Vector(3, 5, 4).toRational shouldBe -BigRational((3*24+5)*1080+4, 1*24*1080)
    -Vector(3, 5, 4).toRational shouldBe BigRational(-((3*24+5)*1080+4), 1*24*1080)
  }

  "fromRational()" should "be correct" in {
    def test(value: Vector): Unit = {
      val rational = value.toRational
      val number = Vector.fromRational(rational)
      number shouldBe value
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
    Vector(3).roundTo(0) shouldBe Vector(3)
    Vector(3).roundTo(1) shouldBe Vector(3)
    Vector(3, 5).roundTo(0) shouldBe Vector(3)
    Vector(3, 5).roundTo(1) shouldBe Vector(3, 5)
    Vector(3, 5).roundTo(2) shouldBe Vector(3, 5)
    -Vector(3, 5).roundTo(0) shouldBe -Vector(3)
    -Vector(3, 12).roundTo(0) shouldBe -Vector(4)
    -Vector(3, 5).roundTo(1) shouldBe -Vector(3, 5)
    -Vector(3, 5).roundTo(2) shouldBe -Vector(3, 5)
    Vector(3, 5, 4).roundTo(0) shouldBe Vector(3)
    Vector(3, 5, 4).roundTo(1) shouldBe Vector(3, 5)
    Vector(3, 5, 4).roundTo(2) shouldBe Vector(3, 5, 4)
    Vector(3, 5, 4).roundTo(3) shouldBe Vector(3, 5, 4)
    Vector(-3, 5, 4).roundTo(0) shouldBe Vector(-3)
    Vector(-3, 5, 4).roundTo(1) shouldBe Vector(-3, 5)
    Vector(-3, 5, 4).roundTo(2) shouldBe Vector(-3, 5, 4)
    Vector(-3, 5, 4).roundTo(3) shouldBe Vector(-3, 5, 4)
    Vector(3, 5, 4, 1).roundTo(2) shouldBe Vector(3, 5, 4)
    Vector(-3, 5, 4, 1).roundTo(2) shouldBe Vector(-3, 5, 4)
    Vector(-3, 5, 4, 37).roundTo(2) shouldBe Vector(-3, 5, 4)
    Vector(-3, 5, 4, 38).roundTo(2) shouldBe Vector(-3, 5, 5)
    Vector(-3, 5, 4, 39).roundTo(2) shouldBe Vector(-3, 5, 5)
  }

  "toString()" should "be correct" in {
    Vector(3).toString shouldBe "3d"
    Vector(3, 5).toString shouldBe "3d5h"
    Vector(3, 5).toString(2) shouldBe "3d5h0p"
    Vector(3, 5).toString(3) shouldBe "3d5h0p0m"
    Vector(3, 5).toString(4) shouldBe "3d5h0p0m0"
    Vector(3, 5, 4).toString shouldBe "3d5h4p"
    Vector(-3, 5, 4).toString shouldBe "-2d18h1076p"
    Vector(3, 5, 4, 1).toString shouldBe "3d5h4p1m"
    Vector(-3, 5, 4, 1).toString shouldBe "-2d18h1075p75m"
    (-Vector(0, 5, 4, 1)).toString shouldBe "-0d5h4p1m"
  }
}
