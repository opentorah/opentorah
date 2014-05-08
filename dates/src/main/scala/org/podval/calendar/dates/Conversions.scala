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


object Conversions {


  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0


  def toJewish(moment: Gregorian.Moment): Jewish.Moment = {
    val hours = moment.time.hours

    val (newDay, newHours) =
      if (hours >= JewishHelper.dayStartHours)
        (moment.day.next, hours - JewishHelper   .dayStartHours) else
        (moment.day     , hours + GregorianHelper.dayStartHours)

    toJewish(newDay).time(newHours, moment.time.parts)
  }


  def fromJewish(moment: Jewish.Moment): Gregorian.Moment = {
    val hours = moment.time.hours

    val (newDay, newHours) =
      if (hours < GregorianHelper.dayStartHours)
        (moment.day.prev, hours + JewishHelper.dayStartHours) else
        (moment.day     , hours - GregorianHelper.dayStartHours)

    fromJewish(newDay).time(newHours, moment.time.parts)
  }


  def fromJewish(day: Jewish   .Day): Gregorian.Day = Gregorian.Day(day.number - GregorianHelper.epoch)


  def toJewish  (day: Gregorian.Day): Jewish   .Day = Jewish   .Day(day.number + GregorianHelper.epoch)
}
