/*
 *  Copyright 2009-2015 dub.
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *  under the License.
 */

package org.podval.calendar.ical

import org.podval.calendar.dates.{Jewish, Gregorian, Conversions}

import java.io.{OutputStream, FileOutputStream}


final class ICalGenerator private(os: OutputStream) {

  import ICal._
  import ICalGenerator.{daysUrl, iconUrl}

  val out = new ICalWriter(os)


  private def writeYear(year: Int) {
    out.print(beginCalendar(
      "-//Podval Group//NONSGML Jewish Calendar//EN",
      Some("Jewish Dates"),
      Some("Jewish Dates, Events and Schedules")
    ))

    var dayG = Gregorian.Year(year).month(Gregorian.Month.January).day(1)
    while (dayG.year.number == year) {
      out.print(day(dayG))
      dayG = dayG.next
    }

    out.print(endCalendar)
  }


  private def day(dayG: Gregorian.Day): Properties = {
    val dayJ = Conversions.toJewish(dayG)
    val monthName: String = dayJ.month.name.name
    val dayNumber = dayJ.numberInMonth
    val summaryText = monthName + " " + dayNumber
    val url = daysUrl + dayJ.year.number + "/" + monthName + "/" + dayNumber

    val result: Properties =
      summary(summaryText) ++
      fullDayDuration(dayG) ++
      googleContent(summaryText, iconUrl, url, 200, 200)

    event(true, result)
  }
}


object ICalGenerator {

  val baseUrl = "http://calendar.podval.org/"
  val daysUrl = baseUrl + "day/"
  val iconUrl = baseUrl + "icon.gif"


  def main(args: Array[String]) {
    val x = Jewish /// XXX why do I need to do this?!
    val y = Gregorian

    new ICalGenerator(new FileOutputStream("/tmp/jc.ics")).writeYear(2013)
  }
}
