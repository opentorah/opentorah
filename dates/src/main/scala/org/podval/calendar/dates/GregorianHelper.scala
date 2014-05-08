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


object GregorianHelper extends Helper {

  private val monthsInYear = 12


  private val daysInNonLeapYear = 365


  // TODO give names to constants?

  override def areYearsPositive: Boolean = false


  override def isLeap(yearNumber: Int): Boolean = (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))


  def firstDay(yearNumber: Int): Int = daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 + (yearNumber - 1)/400 + 1


  def lengthInDays(yearNumber: Int): Int = if (isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear


  override def firstMonth(yearNumber: Int): Int = monthsInYear*(yearNumber - 1) + 1


  override def lengthInMonths(yearNumber: Int): Int = monthsInYear


  override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / monthsInYear + 1


  override def numberInYear(monthNumber: Int): Int =  monthNumber - firstMonth(yearNumber(monthNumber)) + 1


  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  override val dayStartHours = Helper.hoursPerDay - JewishHelper.dayStartHours
}
