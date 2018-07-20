package org.podval.calendar.numbers

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

import BigRational.{one, oneHalf, zero}

final class BigRationalTest extends FlatSpec with GeneratorDrivenPropertyChecks with Matchers {
  val minusThreeHalfs: BigRational = BigRational(-3, 2)

  def nonZeroInt: Gen[Int] = for {
    result <- arbitrary[Int] if result != 0
  } yield result

  def bigInt: Gen[BigInt] = arbitrary[BigInt]

  def nonZeroBigInt: Gen[BigInt] = for {
    result <- arbitrary[BigInt] if result != 0
  } yield result

  def rational: Gen[BigRational] = for {
    numerator <- bigInt
    denominator <- nonZeroBigInt
  } yield BigRational(numerator, denominator)

  def nonZeroRational: Gen[BigRational] = for {
    numerator <- nonZeroBigInt
    denominator <- nonZeroBigInt
  } yield BigRational(numerator, denominator)

  "apply()" should "not allow zero denominator" in {
    forAll(bigInt) { n => assertThrows[ArithmeticException](BigRational(n, 0)) }
  }

  "apply()" should "detect zero numerator" in {
    forAll(nonZeroBigInt) { n => BigRational(0, n) shouldBe zero }
  }

  "apply()" should "handle sign correctly" in {
    forAll(bigInt, nonZeroBigInt) { (n, d) => BigRational(-n, -d) shouldBe BigRational(n, d) }
  }

  "signum" should "be stored in the numerator" in {
    forAll(bigInt, nonZeroBigInt) { (n, d) => BigRational(n, d).signum shouldBe BigRational(n, d).numerator.signum }
  }

  "apply()" should "simplify via GCD correctly" in {
    forAll(bigInt, nonZeroBigInt, nonZeroInt) { (n, d, c) => BigRational(n*c, d*c) shouldBe BigRational(n, d) }
  }

  "apply(numerator, denominator)" should "be identity" in {
    forAll(rational) { r => BigRational(r.numerator, r.denominator) shouldBe r }
  }

  "apply(toString)" should "be identity" in {
    forAll(rational) { r => BigRational(r.toString) shouldBe r }
  }

  "signum() and <" should "be consistent" in {
    forAll(rational) { r =>
      (r.signum == -1) == (r < zero) shouldBe true
      (r.signum ==  0) == (r == zero) shouldBe true
      (r.signum ==  1) == (r > zero) shouldBe true
    }
  }

  "abs()" should "be determined by signum()" in {
    forAll(rational) { r => r.abs shouldBe (if (r.signum < 0) -r else r) }
  }

  "abs()" should "be idempotent" in {
    forAll(rational) { r => r.abs.abs shouldBe r.abs }
  }

  "0" should "fixed point of unary -" in {
    -zero shouldBe zero
  }

  "unary -" should "be self-inverse" in {
    forAll(rational) { r => -(-r) shouldBe r }
  }

  "unary -" should "be inverse of +" in {
    forAll(rational) { r => r + -r shouldBe zero }
  }

  "invert()" should "be self-inverse" in {
    forAll(nonZeroRational) { r => r.invert.invert shouldBe r }
  }

  "+" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l + r shouldBe r + l }
  }

  "+" should "have 0 as unit" in {
    forAll(rational) { r => r + zero shouldBe r }
    forAll(rational) { r => zero + r shouldBe r }
  }

  "+ and >" should "be consistent" in {
    forAll(rational, rational) { (l, r) => (r < r + l) shouldBe l.signum > 0 }
  }

  "-" should "be inverse of +" in {
    forAll(rational) { r => r - r shouldBe zero }
    forAll(rational, rational) { (l, r) => l + r - r shouldBe l }
  }

  "-, unary - and +" should "be related correctly" in {
    forAll(rational, rational) { (l, r) => l - r shouldBe l + (-r) }
  }

  "*" should "be commutative" in {
    forAll(rational, rational) { (l, r) => l * r shouldBe r * l }
  }

  "*" should "have 1 as unit" in {
    forAll(rational) { r => r * one shouldBe r }
    forAll(rational) { r => one * r shouldBe r }
  }

  "1" should "fixed point of invert()" in {
    one.invert shouldBe one
  }

  "/" should "be inverse of *" in {
    forAll(rational, nonZeroRational) { (l, r) => l * r / r shouldBe l }
    forAll(nonZeroRational) { r => r / r shouldBe one }
  }

  "/, * and invert()" should "be consistent" in {
    forAll(rational, nonZeroRational) { (l, r) => l / r shouldBe l * r.invert }
  }

  "fraction()" should "be idempotent" in {
    forAll(rational) { r => r.fraction.fraction shouldBe r.fraction }
  }

  "whole()+fraction()" should "be identity where defined" in {
    forAll(rational) { r =>
      try {
        BigRational(r.whole) + r.fraction shouldBe r
      } catch { case _: ArithmeticException => /* whole() is too big */ }
    }
  }

  "toString()" should "be correct" in {
    zero.toString shouldBe "0/1"
    oneHalf.toString shouldBe "1/2"
    minusThreeHalfs.toString shouldBe "-3/2"
  }

  "==" should "be correct" in {
    zero == zero shouldBe true
    zero == BigRational(0) shouldBe true
    zero == BigRational(1) shouldBe false
  }

  "compare()" should "be correct" in {
    zero < oneHalf shouldBe true
    oneHalf <= one shouldBe true
    one > minusThreeHalfs shouldBe true
  }

  "signum()" should "be correct" in {
    zero.signum shouldBe 0
    oneHalf.signum shouldBe 1
    minusThreeHalfs.signum shouldBe -1
  }

  "unary -" should "be correct" in {
    -zero shouldBe zero
    -oneHalf shouldBe BigRational(-1, 2)
    minusThreeHalfs.abs shouldBe BigRational(3, 2)
  }

  "invert()" should "be correct" in {
    assertThrows[ArithmeticException](zero.invert)
    oneHalf.invert shouldBe one+one
    minusThreeHalfs.invert shouldBe BigRational(-2, 3)
  }

  "+" should "be correct" in {
    zero+zero shouldBe zero
    oneHalf + minusThreeHalfs shouldBe -one
  }

  "-" should "be correct" in {
    zero-zero shouldBe zero
    oneHalf-minusThreeHalfs shouldBe BigRational(2)
  }

  "*" should "be correct" in {
    zero*zero shouldBe zero
    oneHalf*oneHalf shouldBe one/BigRational(4)
    minusThreeHalfs*oneHalf shouldBe BigRational(-3, 4)
  }

  "/" should "be correct" in {
    assertThrows[ArithmeticException](zero/zero)
    oneHalf/oneHalf shouldBe one
    minusThreeHalfs/oneHalf shouldBe -(one+one+one)
  }

  "whole() and fraction()" should "be correct" in {
    def mult(r: BigRational, n: Int): BigRational = r * BigRational(n)
    val days: Int = 1
    val hours: Int = 2
    val parts: Int = 3
    val time: BigRational = BigRational(((days*24)+hours)*1080+parts, 1*24*1080)

    time.whole shouldBe days

    val remainderhp: BigRational = time.fraction
    mult(remainderhp, 24).whole shouldBe hours

    val remainderp: BigRational = mult(remainderhp, 24).fraction
    mult(remainderp, 1080).whole shouldBe parts

    val remainder: BigRational = mult(remainderp, 1080).fraction
    remainder.numerator shouldBe 0

    zero.whole shouldBe 0
    zero.fraction shouldBe zero

    oneHalf.whole shouldBe 0
    oneHalf.fraction shouldBe oneHalf

    minusThreeHalfs.whole shouldBe -1
    minusThreeHalfs.fraction shouldBe -oneHalf
  }
}
