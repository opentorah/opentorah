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


trait CalendarHelper {


  val firstDayDayOfWeek: Int


  final def dayOfWeek(dayNumber: Int): Int = ((dayNumber + firstDayDayOfWeek - 1 - 1) % CalendarHelper.daysPerWeek) + 1


  def isLeap(yearNumber: Int): Boolean


  def firstMonth(yearNumber: Int): Int


  def lengthInMonths(yearNumber: Int): Int


  def yearNumberOfMonth(monthNumber: Int): Int


  def numberInYearOfMonth(monthNumber: Int): Int
}



object CalendarHelper {

  val hoursPerDay = 24

  require(hoursPerDay % 2 == 0)


  val hoursPerHalfDay = hoursPerDay / 2


  val partsPerHour = 1080


  val minutesPerHour = 60

  require(partsPerHour % minutesPerHour == 0)


  val partsPerMinute = partsPerHour / minutesPerHour


  val daysPerWeek: Int = 7
}
