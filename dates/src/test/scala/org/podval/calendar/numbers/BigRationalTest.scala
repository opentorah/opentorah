package org.podval.calendar.numbers

import org.scalatest.FlatSpec
import org.scalatest.Matchers
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
    BigRational(0, 7) should be(zero)
  }

  "apply()" should "handle sign correctly" in {
    BigRational(-1, 1) should be (-one)
    BigRational(1, -1) should be (-one)
    BigRational(1, 1) should be (one)
  }

  "apply()" should "simplify via GCD correctly" in {
    BigRational(-13, -26) should be (oneHalf)
    BigRational(1, 1) should be (one)
  }

  "toString()" should "be correct" in {
    zero.toString should be ("0/1")
    oneHalf.toString should be("1/2")
    minusThreeHalfs.toString should be ("-3/2")
  }

  "apply(String)" should "be inverse of toString()" in {
    forAll(rational) { r => BigRational(r.toString) should be (r) }
  }

  "signum()" should "be correct" in {
    zero.signum should be(0)
    oneHalf.signum should be(1)
    minusThreeHalfs.signum should be(-1)
  }

  "signum() and <" should "be consistent" in {
    forAll(rational) { r =>
      if (r.signum == -1) r < zero should be (true)
      if (r.signum ==  0) r == zero should be (true)
      if (r.signum ==  1) r > zero should be (true)
    }
  }

  "signum() and abs()" should "be consistent" in {
    forAll(rational) { r =>  r.abs should be (if (r.signum == -1) -r else r) }
  }

  "abs()" should "be correct" in {
    zero.abs  should be(zero)
    oneHalf.abs should be(oneHalf)
    minusThreeHalfs.abs should be(-minusThreeHalfs)
  }

  "abs()" should "be idempotent" in {
    forAll(rational) { r => r.abs.abs should be (r.abs) }
  }

  "abs()" should "be determined by signum()" in {
    forAll(rational) { r => if (r.signum < 0) r.abs should be (-r) else r.abs should be (r) }
  }

  "unary_-()" should "be correct" in {
    -zero should be(zero)
    -oneHalf should be(BigRational(-1, 2))
    minusThreeHalfs.abs should be(BigRational(3, 2))
  }

  "unary_-()" should "be self-inverse" in {
    forAll(rational) { r => -(-r) should be (r) }
  }

  "unary_-()" should "be inverse of +()" in {
    forAll(rational) { r => r + -r should be (zero) }
  }

  "invert()" should "be correct" in {
    assertThrows[ArithmeticException](zero.invert)
    oneHalf.invert should be (one+one)
    minusThreeHalfs.invert should be (BigRational(-2, 3))
  }

  "invert()" should "be idempotent" in {
    forAll(nonZeroRational) { r => r.invert.invert should be (r) }
  }

  "+()" should "be correct" in {
    zero+zero should be (zero)
    oneHalf + minusThreeHalfs should be (-one)
  }

  "+()" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l + r should be (r + l) }
  }

  "+()" should "have 0 as unit" in {
    forAll(rational) { r => r + zero should be (r) }
    forAll(rational) { r => zero + r should be (r) }
  }

  "+() and >" should "be consistent" in {
    forAll(rational, rational) { (l, r) => r < r + l should be (l.signum > 0) }
  }

  "-()" should "be correct" in {
    zero-zero should be (zero)
    oneHalf-minusThreeHalfs should be(one*2)
  }

  "-()" should "be inverse of +()" in {
    forAll(rational) { r => r - r should be (zero) }
    forAll(rational, rational) { (l, r) => l + r - r should be (l) }
  }

  "-(), unary_-() and +()" should "be related correctly" in {
    forAll(rational, rational) { (l, r) => l - r should be (l + (-r)) }
  }

  "*(Int)" should "be correct" in {
    zero*3 should be (zero)
    oneHalf*(-2) should be (-one)
    minusThreeHalfs*2 should be (zero-one-one-one)
  }

  "*(BigRational)" should "be correct" in {
    zero*zero should be (zero)
    oneHalf*oneHalf should be (one/4)
    minusThreeHalfs*oneHalf should be (BigRational(-3, 4))
  }

  "*(BigRational)" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l * r should be (r * l) }
  }

  "*(BigRational)" should "have 1 as unit" in {
    forAll(rational) { r => r * one should be (r) }
    forAll(rational) { r => one * r should be (r) }
  }

  "/(Int)" should "be correct" in {
    zero/3 should be (zero)
    oneHalf/(-2) should be (BigRational(-1, 4))
    minusThreeHalfs/2 should be (BigRational(-3, 4))
  }

  "/(BigRational)" should "be correct" in {
    assertThrows[ArithmeticException](zero/zero)
    oneHalf/oneHalf should be (one)
    minusThreeHalfs/oneHalf should be (-(one+one+one))
  }

  "/(BigRational)" should "be inverse of *(BigRational)" in {
    forAll(rational, nonZeroRational) { (l, r) => l * r / r should be (l) }
    forAll(nonZeroRational) { r => r / r should be (one) }
  }

  "/(BigRational), inverse() and *(BigRational)" should "be related correctly" in {
    forAll(rational, nonZeroRational) { (l, r) => l / r should be (l * r.invert) }
  }

  "wholeAndFraction()" should "be correct" in {
    val days: Int = 1
    val hours: Int = 2
    val parts: Int = 3
    val time: BigRational = BigRational(((days*24)+hours)*1080+parts, 1*24*1080)

    val daysO: Int = time.whole
    val remainderhp: BigRational = time.fraction
    daysO should be (days)

    val hoursO: Int = (remainderhp*24).whole
    val remainderp: BigRational = (remainderhp*24).fraction
    hoursO should be (hours)

    val partsO: Int = (remainderp*1080).whole
    val remainder: BigRational = (remainderp*1080).fraction
    partsO should be (parts)
    remainder.numerator should be (0)

    zero.whole should be (0)
    zero.fraction should be (zero)

    oneHalf.whole should be (0)
    oneHalf.fraction should be (oneHalf)

    minusThreeHalfs.whole should be (-1)
    minusThreeHalfs.fraction should be (-oneHalf)
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
}
