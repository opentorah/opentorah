package org.podval.calendar.numbers

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import BigRational.{one, oneHalf, zero}

final class BigRationalTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  val minusThreeHalfs: BigRational = BigRational(-3, 2)

  def rational: Gen[BigRational] = for {
    numerator <- arbitrary[BigInt]
    denominator <- arbitrary[BigInt] if denominator != 0
  } yield BigRational(numerator, denominator)

  def nonZeroRational: Gen[BigRational] = for {
    numerator <- arbitrary[BigInt] if numerator != 0
    denominator <- arbitrary[BigInt] if denominator != 0
  } yield BigRational(numerator, denominator)

  "apply()" should "detect zero denominator" in {
    assertThrows[ArithmeticException](BigRational(1, 0))
    assertThrows[ArithmeticException](BigRational(0, 0))
  }

  "apply()" should "detect zero numerator" in {
    BigRational(0, 7) shouldBe zero
  }

  "apply()" should "handle sign correctly" in {
    BigRational(-1, 1) shouldBe -one
    BigRational(1, -1) shouldBe -one
    BigRational(1, 1) shouldBe one
  }

  "apply()" should "simplify via GCD correctly" in {
    BigRational(-13, -26) shouldBe oneHalf
    BigRational(1, 1) shouldBe one
  }

  "toString()" should "be correct" in {
    zero.toString shouldBe "0/1"
    oneHalf.toString shouldBe "1/2"
    minusThreeHalfs.toString shouldBe "-3/2"
  }

  "apply(String)" should "be inverse of toString()" in {
    forAll(rational) { r => BigRational(r.toString) shouldBe r }
  }

  "signum()" should "be correct" in {
    zero.signum shouldBe 0
    oneHalf.signum shouldBe 1
    minusThreeHalfs.signum shouldBe -1
  }

  "signum() and <" should "be consistent" in {
    forAll(rational) { r =>
      if (r.signum == -1) r < zero shouldBe true
      if (r.signum ==  0) r == zero shouldBe true
      if (r.signum ==  1) r > zero shouldBe true
    }
  }

  "abs()" should "be determined by signum()" in {
    forAll(rational) { r => r.abs shouldBe (if (r.signum < 0) -r else r) }
  }

  "abs()" should "be correct" in {
    zero.abs  shouldBe zero
    oneHalf.abs shouldBe oneHalf
    minusThreeHalfs.abs shouldBe -minusThreeHalfs
  }

  "abs()" should "be idempotent" in {
    forAll(rational) { r => r.abs.abs shouldBe r.abs }
  }

  "unary_-()" should "be correct" in {
    -zero shouldBe zero
    -oneHalf shouldBe BigRational(-1, 2)
    minusThreeHalfs.abs shouldBe BigRational(3, 2)
  }

  "unary_-()" should "be self-inverse" in {
    forAll(rational) { r => -(-r) shouldBe r }
  }

  "unary_-()" should "be inverse of +()" in {
    forAll(rational) { r => r + -r shouldBe zero }
  }

  "invert()" should "be correct" in {
    assertThrows[ArithmeticException](zero.invert)
    oneHalf.invert shouldBe one+one
    minusThreeHalfs.invert shouldBe BigRational(-2, 3)
  }

  "invert()" should "be idempotent" in {
    forAll(nonZeroRational) { r => r.invert.invert shouldBe r }
  }

  "+()" should "be correct" in {
    zero+zero shouldBe zero
    oneHalf + minusThreeHalfs shouldBe -one
  }

  "+()" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l + r shouldBe r + l }
  }

  "+()" should "have 0 as unit" in {
    forAll(rational) { r => r + zero shouldBe r }
    forAll(rational) { r => zero + r shouldBe r }
  }

  "+() and >" should "be consistent" in {
    forAll(rational, rational) { (l, r) => r < r + l shouldBe l.signum > 0 }
  }

  "-()" should "be correct" in {
    zero-zero shouldBe zero
    oneHalf-minusThreeHalfs shouldBe one*2
  }

  "-()" should "be inverse of +()" in {
    forAll(rational) { r => r - r shouldBe zero }
    forAll(rational, rational) { (l, r) => l + r - r shouldBe l }
  }

  "-(), unary_-() and +()" should "be related correctly" in {
    forAll(rational, rational) { (l, r) => l - r shouldBe l + (-r) }
  }

  "*(Int)" should "be correct" in {
    zero*3 shouldBe zero
    oneHalf*(-2) shouldBe -one
    minusThreeHalfs*2 shouldBe zero-one-one-one
  }

  "*(BigRational)" should "be correct" in {
    zero*zero shouldBe zero
    oneHalf*oneHalf shouldBe one/4
    minusThreeHalfs*oneHalf shouldBe BigRational(-3, 4)
  }

  "*(BigRational)" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l * r shouldBe r * l }
  }

  "*(BigRational)" should "have 1 as unit" in {
    forAll(rational) { r => r * one shouldBe r }
    forAll(rational) { r => one * r shouldBe r }
  }

  "/(Int)" should "be correct" in {
    zero/3 shouldBe zero
    oneHalf/(-2) shouldBe  BigRational(-1, 4)
    minusThreeHalfs/2 shouldBe BigRational(-3, 4)
  }

  "/(BigRational)" should "be correct" in {
    assertThrows[ArithmeticException](zero/zero)
    oneHalf/oneHalf shouldBe one
    minusThreeHalfs/oneHalf shouldBe -(one+one+one)
  }

  "/(BigRational)" should "be inverse of *(BigRational)" in {
    forAll(rational, nonZeroRational) { (l, r) => l * r / r shouldBe l }
    forAll(nonZeroRational) { r => r / r shouldBe one }
  }

  "/(BigRational), inverse() and *(BigRational)" should "be related correctly" in {
    forAll(rational, nonZeroRational) { (l, r) => l / r shouldBe l * r.invert }
  }

  "whole() and fraction()" should "be correct" in {
    val days: Int = 1
    val hours: Int = 2
    val parts: Int = 3
    val time: BigRational = BigRational(((days*24)+hours)*1080+parts, 1*24*1080)

    time.whole shouldBe days

    val remainderhp: BigRational = time.fraction
    (remainderhp*24).whole shouldBe hours

    val remainderp: BigRational = (remainderhp*24).fraction
    (remainderp*1080).whole shouldBe parts

    val remainder: BigRational = (remainderp*1080).fraction
    remainder.numerator shouldBe 0

    zero.whole shouldBe 0
    zero.fraction shouldBe zero

    oneHalf.whole shouldBe 0
    oneHalf.fraction shouldBe oneHalf

    minusThreeHalfs.whole shouldBe -1
    minusThreeHalfs.fraction shouldBe -oneHalf
  }

  "==()" should "be correct" in {
    assert(zero == zero)
    assert(zero == BigRational(0))
    assert(zero != BigRational(1))
  }

  "compare()" should "be correct" in {
    assert(zero < oneHalf)
    assert(oneHalf <= one)
    assert(one > minusThreeHalfs)
  }

  "whole()+fraction()" should "be identity where defined" in {
    forAll(rational) { r =>
      try {
        BigRational(r.whole) + r.fraction shouldBe r
      } catch { case e: ArithmeticException => /* whole() is too big */ }
    }
  }
}
