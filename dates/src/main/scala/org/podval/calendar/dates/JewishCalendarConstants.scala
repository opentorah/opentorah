/*
 * Copyright 2011-2013 Podval Group.
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


object JewishCalendarConstants {

  val YearsInCycle = 19


  val LeapYears = Set(3, 6, 8, 11, 14, 17, 19)


  def isLeap(numberInCycle: Int) = LeapYears.contains(numberInCycle)


  def lengthInMonths(numberInCycle: Int): Int = if (isLeap(numberInCycle)) MonthsInLeapYear else MonthsInNonLeapYear


  val MonthsInNonLeapYear = 12


  val MonthsInLeapYear = MonthsInNonLeapYear + 1


  val MonthsBeforeYearInCycle = ((1 to YearsInCycle) map (lengthInMonths(_))).scanLeft(0)(_ + _)


  val MonthsInCycle = MonthsBeforeYearInCycle.last
}
