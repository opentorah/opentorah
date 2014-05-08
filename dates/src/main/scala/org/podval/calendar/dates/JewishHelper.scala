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


object JewishHelper extends Helper {

  private val yearsInCycle = 19


  private val leapYears = Set(3, 6, 8, 11, 14, 17, 19)


  private val monthsBeforeYearInCycle = ((1 to yearsInCycle) map (lengthInMonths(_))).scanLeft(0)(_ + _)


  private val monthsInCycle = monthsBeforeYearInCycle.last


  override def areYearsPositive: Boolean = true


  override def isLeap(yearNumber: Int) = leapYears.contains(numberInCycle(yearNumber))


  override def firstMonth(yearNumber: Int): Int = monthsInCycle*(cycle(yearNumber) - 1) + firstMonthInCycle(yearNumber)


  override def lengthInMonths(yearNumber: Int): Int = if (isLeap(yearNumber)) 13 else 12


  def cycle(yearNumber: Int): Int = ((yearNumber - 1) / yearsInCycle) + 1


  def numberInCycle(yearNumber: Int): Int = ((yearNumber - 1) % yearsInCycle) + 1


  override def yearNumber(monthNumber: Int): Int = {
    val cycleOfMonth = ((monthNumber - 1) / monthsInCycle) + 1
    val yearsBeforeCycle = (cycleOfMonth - 1) * yearsInCycle
    val yearMonthIsInCycle = monthsBeforeYearInCycle.count(_ < numberInCycleOfMonth(monthNumber))
    yearsBeforeCycle + yearMonthIsInCycle
  }


  override def numberInYear(monthNumber: Int): Int = numberInCycleOfMonth(monthNumber) - firstMonthInCycle(yearNumber(monthNumber)) + 1


  private def firstMonthInCycle(yearNumber: Int): Int = monthsBeforeYearInCycle(numberInCycle(yearNumber) - 1) + 1


  private def numberInCycleOfMonth(monthNumber: Int): Int = ((monthNumber - 1) % monthsInCycle) + 1


  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  override val dayStartHours = 18
}
