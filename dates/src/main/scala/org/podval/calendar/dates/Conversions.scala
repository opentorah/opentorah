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


object Conversions {

  private val jewishDayStartHours = 18


  private val jewishDayLeftHours = CalendarHelper.hoursPerDay - jewishDayStartHours


  def toJewish(moment: GregorianCalendar.Moment): JewishCalendar.Moment = {
    val hours = moment.time.hours

    val (newDay, newHours) =
      if (hours >= jewishDayStartHours)
        (moment.day.next, hours - jewishDayStartHours) else
        (moment.day     , hours + jewishDayLeftHours)

    toJewish(newDay).time(newHours, moment.time.parts)
  }


  def fromJewish(day: JewishCalendar   .Day): GregorianCalendar.Day = GregorianCalendar.Day(day.number - GregorianCalendarHelper.epoch)


  def toJewish  (day: GregorianCalendar.Day): JewishCalendar   .Day = JewishCalendar   .Day(day.number + GregorianCalendarHelper.epoch)
}
