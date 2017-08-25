package org.podval.calendar.dates.time

import org.podval.calendar.numbers.BigRational
import org.scalatest.FlatSpec

@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class TimeNumberSystemTest extends FlatSpec {

  "ranges and multipliers" should "be correct" in {
    import SimpleTimeNumberSystem.{range, multiplier}

    assertResult(  24)(range(0))
    assertResult(1080)(range(1))
    assertResult(  76)(range(2))

    assertResult(1           )(multiplier(0))
    assertResult(1*24        )(multiplier(1))
    assertResult(1*24*1080   )(multiplier(2))
    assertResult(1*24*1080*76)(multiplier(3))
  }

  "toRational()" should "be correct" in {
    import SimpleTimeNumberSystem.newInterval

    val days = newInterval(false, List(3))
    assertResult("3d")(days.toString)
    assertResult(BigRational(3, 1))(days.toRational)

    val hours = newInterval(false, List(3, 5))
    assertResult("3d5h")(hours.toString)
    assertResult(BigRational(3*24+5, 1*24))(hours.toRational)

    val parts = newInterval(false, List(3, 5, 4))
    assertResult("3d5h4p")(parts.toString)
    assertResult(BigRational((3*24+5)*1080+4, 1*24*1080))(parts.toRational)

    val moments = newInterval(false, List(3, 5, 4, 1))
    assertResult("3d5h4p1m")(moments.toString)
    assertResult(BigRational(((3*24+5)*1080+4)*76+1, 1*24*1080*76))(moments.toRational)
  }
}
