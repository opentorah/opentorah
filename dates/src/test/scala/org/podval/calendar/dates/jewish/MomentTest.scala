package org.podval.calendar.dates.jewish

import org.scalatest.FlatSpec
import Jewish.interval


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class MomentTest extends FlatSpec {

  "Moment components" should "be correct" in {
    components2moment2components( 0, 18,   0)
    components2moment2components( 0,  9, 204)
    components2moment2components( 0, 15, 589)
    components2moment2components(29, 12, 793)
    components2moment2components( 1,  5, 204)
  }


  private def components2moment2components(days: Int, hours: Int, parts: Int) {
    val moment = interval.days(days).hours(hours).parts(parts)

    assertResult(days)(moment.days)
    assertResult(hours)(moment.hours)
    assertResult(parts)(moment.parts)
  }
}
