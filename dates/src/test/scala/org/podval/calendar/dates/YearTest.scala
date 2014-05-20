/*
 * Copyright 2011-2014 Podval Group.
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

package org.podval.calendar.dates

import org.scalatest.FlatSpec


@org.junit.runner.RunWith(classOf[org.scalatest.junit.JUnitRunner])
final class YearTest extends FlatSpec {

  private val years = (1 to 6000) map (Jewish.Year(_))


  "A Jewish year" should "have a valid kind" in {
    years foreach (_.kind)
  }


  it should "belong to the year it was retrieved from" in {
    for (year <- years; month <- (1 to year.lengthInMonths))
      assertResult(year)(year.month(month).year)
  }
}
