package org.podval.calendar.numbers

import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class BigRationalTest extends FlatSpec {
  // TODO add real tests

  "whole()" should "be correct" in {
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
    assertResult(true)(remainder.isZero)
  }
}
