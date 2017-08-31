package org.podval.calendar.time

import org.scalatest.FlatSpec
import org.podval.calendar.numbers.BigRational

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class TimeNumberSystemTest extends FlatSpec {

  "ranges" should "be correct" in {
    import SimpleTimeNumberSystem.range

    assertResult(  24)(range(0))
    assertResult(1080)(range(1))
    assertResult(  76)(range(2))
  }

  "toRational()" should "be correct" in {
    import SimpleTimeNumberSystem.Interval

    val days = Interval(false, 3)
    assertResult("3d")(days.toString)
    assertResult(BigRational(3, 1))(days.toRational)

    val hours = Interval(false, 3, 5)
    assertResult("3d5h")(hours.toString)
    assertResult(BigRational(3*24+5, 1*24))(hours.toRational)

    val parts = Interval(false, 3, 5, 4)
    assertResult("3d5h4p")(parts.toString)
    assertResult(BigRational((3*24+5)*1080+4, 1*24*1080))(parts.toRational)

    val moments = Interval(false, 3, 5, 4, 1)
    assertResult("3d5h4p1m")(moments.toString)
    assertResult(BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76))(moments.toRational)
  }

  "fromRational()" should "be correct" in {
    // TODO use Interval, not newInterval...
    import SimpleTimeNumberSystem.{Interval, fromRational, newInterval}

    def test(value: Interval): Unit =
      assertResult(value)(newInterval(fromRational(value.toRational)))

    test(newInterval(false, List(3)))
    test(newInterval(false, List(3, 5)))
    test(newInterval(false, List(3, 5, 4)))
    test(newInterval(false, List(3, 5, 4, 1)))
  }
}
