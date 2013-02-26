/*
 *  Copyright 2009-2013 dub.
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

package org.podval.calendar.ical;

import org.podval.calendar.dates.{Jewish, Gregorian, Conversions}

import java.io.{OutputStream, FileOutputStream}


final class ICalGenerator private(os: OutputStream) {

  val out = new ICalWriter(os)


  private def writeYear(year: Int) {
      out.beginCalendar("-//Podval Group//NONSGML Jewish Calendar//EN", "Jewish Dates", "Jewish Dates, Events and Schedules")

      var dayG = Gregorian.Day(year, 1, 1)
      while (dayG.year.number == year) {
        writeDay(dayG)
        dayG = dayG.next
      }

      out.endCalendar
  }


  private def writeDay(dayG: Gregorian.Day) {
    val dayJ = Conversions.toJewish(dayG)
    val monthName = toMonthName(dayJ.month.name)
    val dayNumber = dayJ.numberInMonth
    val summary = monthName + " " + dayNumber
    val url = ICalGenerator.daysUrl + dayJ.year.number + "/" + monthName + "/" + dayNumber

    out.beginEvent
    out.writeSummary(summary)
    out.writeFullDayDuration(dayG)
    out.addGoggleContent(summary, ICalGenerator.iconUrl, url, 200, 200)
    out.endEvent
  }


  private def toMonthName(month: Jewish.Month.Name): String = {
    import Jewish.Month._
    month match {
      case Tishrei   => "Тишрей"
      case Marheshvan=> "Мар-Хешван"
      case Kislev    => "Кислев"
      case Teves     => "Тевес"
      case Shvat     => "Шват"
      case Adar      => "Адар"
      case AdarI     => "Адар I"
      case AdarII    => "Адар II"
      case Nisan     => "Нисан"
      case Iyar      => "Ияр"
      case Sivan     => "Сиван"
      case Tammuz    => "Таммуз"
      case Av        => "Ав"
      case Elul      => "Элул"
    }
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
