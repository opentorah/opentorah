/*
 * Copyright 2011 Podval Group.
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

import org.junit.{Before, Test, Assert}

import JewishCalendar.Year


final class YearTest {

  private val x = JewishCalendar


  private val years = (1 to 6000) map (Year(_))


  @Test
  def kind = years foreach (_.kind)


  @Test
  def month = for (year <- years; month <- (1 to year.lengthInMonths))
    Assert.assertEquals(year, year.month(month).year)
}
