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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite


@RunWith(classOf[JUnitRunner])
final class YearTest extends FunSuite {

  private val x = Jewish


  private val years = (1 to 6000) map (Jewish.Year(_))


  test("All Jewish years should have a valid kind") {
    years foreach (_.kind)
  }


  test("Year of the month of a year should be the year we started from") {
    for (year <- years; month <- (1 to year.lengthInMonths))
      assertResult(year)(year.month(month).year)
  }
}
