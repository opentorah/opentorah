/*
 * Copyright 2014 Podval Group.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.podval.calendar.dates;

import org.scalatest.FlatSpec

import Jewish.{Moment, Day, Month, day}


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class MomentTest extends FlatSpec {

  "Moment components" should "be correct" in {
    components2moment2components( 0, 18,   0)
    components2moment2components( 0,  9, 204)
    components2moment2components( 0, 15, 589)
    components2moment2components(29, 12, 793)

    assert(day(2).nightHours(5).parts(204) == Moment(1, 5, 0, 204))
  }


  private def components2moment2components(days: Int, hours: Int, parts: Int) {
    val moment = Moment(days, hours, 0, parts)

    assertResult(days)(moment.days)
    assertResult(days)(moment.day.number - 1)
    assertResult(hours)(moment.hours)
    assertResult(parts)(moment.partsWithMinutes)
  }
}
