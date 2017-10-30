package org.podval.calendar.numbers

import org.scalatest.FlatSpec
import BigRational.{zero, oneHalf, one}

final class BigRationalTest extends FlatSpec {
  val minusThreeHalfs: BigRational = BigRational(-3, 2)

  "apply()" should "be correct" in {
    assertThrows[ArithmeticException](BigRational(1, 0))
    assertResult(-one)(BigRational(-1, 1))
    assertResult(-one)(BigRational(1, -1))
  }

  "toString()" should "be correct" in {
    assertResult("0/1")(zero.toString)
    assertResult("1/2")(oneHalf.toString)
    assertResult("-3/2")(minusThreeHalfs.toString)
  }

  "signum()" should "be correct" in {
    assertResult(0)(zero.signum)
    assertResult(1)(oneHalf.signum)
    assertResult(-1)(minusThreeHalfs.signum)
  }

  "abs()" should "be correct" in {
    assertResult(zero)(zero.abs)
    assertResult(oneHalf)(oneHalf.abs)
    assertResult(-minusThreeHalfs)(minusThreeHalfs.abs)
  }

  "unary_-()" should "be correct" in {
    assertResult(zero)(-zero)
    assertResult(BigRational(-1, 2))(-oneHalf)
    assertResult(BigRational(3, 2))(minusThreeHalfs.abs)
  }

  "+()" should "be correct" in {
    assertResult(zero)(zero+zero)
    assertResult(-one)(oneHalf+minusThreeHalfs)
  }

  "-()" should "be correct" in {
    assertResult(zero)(zero-zero)
    assertResult(one*2)(oneHalf-minusThreeHalfs)
  }

  "invert()" should "be correct" in {
    assertThrows[ArithmeticException](zero.invert)
    assertResult(one+one)(oneHalf.invert)
    assertResult(BigRational(-2, 3))(minusThreeHalfs.invert)
  }

  "*(Int)" should "be correct" in {
    assertResult(zero)(zero*3)
    assertResult(-one)(oneHalf*(-2))
    assertResult(zero-one-one-one)(minusThreeHalfs*2)
  }

  "*(BigRational)" should "be correct" in {
    assertResult(zero)(zero*zero)
    assertResult(one/4)(oneHalf*oneHalf)
    assertResult(BigRational(-3, 4))(minusThreeHalfs*oneHalf)
  }

  "/(Int)" should "be correct" in {
    assertResult(zero)(zero/3)
    assertResult(BigRational(-1, 4))(oneHalf/(-2))
    assertResult(BigRational(-3, 4))(minusThreeHalfs/2)
  }

  "/(BigRational)" should "be correct" in {
    assertThrows[ArithmeticException](zero/zero)
    assertResult(one)(oneHalf/oneHalf)
    assertResult(-(one+one+one))(minusThreeHalfs/oneHalf)
  }

  "wholeAndFraction()" should "be correct" in {
    val days: Int = 1
    val hours: Int = 2
    val parts: Int = 3
    val time: BigRational = BigRational(((days*24)+hours)*1080+parts, 1*24*1080)

    val (daysO: Int, remainderhp: BigRational) = time.wholeAndFraction
    assertResult(days)(daysO)

    val (hoursO: Int, remainderp: BigRational) = (remainderhp*24).wholeAndFraction
    assertResult(hours)(hours)

    val (partsO: Int, remainder: BigRational) = (remainderp*1080).wholeAndFraction
    assertResult(parts)(partsO)
    assert(remainder == 0)

    assertResult((0, zero))(zero.wholeAndFraction)
    assertResult((0, oneHalf))(oneHalf.wholeAndFraction)

    assertResult((-1, -oneHalf))(minusThreeHalfs.wholeAndFraction)
  }

  "==()" should "be correct" in {
    assert(zero == zero)
    assert(zero == 0)
    assert(zero != 1)
  }

  "compare()" should "be correct" in {
    assert(zero < oneHalf)
    assert(oneHalf <= one)
    assert(one > minusThreeHalfs)
  }
}
