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

package org.podval.calendar.ical

import org.podval.calendar.dates.Gregorian.Day

import java.io.{OutputStream, PrintStream}


final class ICalWriter(os: OutputStream) {

  val out = new PrintStream(os)


  def beginCalendar(prodId: String, name: String, description: String) {
    println("BEGIN", "VCALENDAR")
    println("PRODID", prodId)
    println("VERSION", "2.0")
    println("CALSCALE", "GREGORIAN")
    println("METHOD", "PUBLISH")

    if (name != null) {
      println("X-WR-CALNAME", name)
    }

    if (description != null) {
      println("X-WR-CALDESC", description)
    }
  }


  def endCalendar = println("END", "VCALENDAR")


  def beginEvent: Unit = beginEvent(true)


  def beginEvent(transparent: Boolean): Unit = {
      println("BEGIN", "VEVENT")
      println("CLASS", "PUBLIC")
      println("STATUS", "CONFIRMED")
      println("TRANSP", if (transparent) "TRANSPARENT" else "OPAQUE")
  }


  def writeSummary(summary: String) = println("SUMMARY", summary)


  def writeFullDayDuration(day: Day) {
    println("DTSTART;VALUE=DATE", toString(day))
/////    println("DTEND;VALUE=DATE", toString(day.next))
    println("DURATION", "P1D")
  }


  private def toString(day: Day): String = {
    val result = new StringBuffer()

    result.append(day.year.number)
    result.append(pad2digits(day.month.numberInYear))
    result.append(pad2digits(day.numberInMonth))

    result.toString
  }


  private def pad2digits(what: Int): String = (if (what < 10) "0" else "") + what


////    public void beginEvent() {
////        println("CATEGORIES", "Holidays");
////        println("URL;VALUE=URI", "http://lwhjsdgfjhf");
////        println("DTSTAMP", "2061121T044202Z");
////        println("UID", "asdhjgd-wjks=-f");
////    }


  def addGoggleContent(
      title: String,
      icon: String,
      url: String,
      width: Int,
      height: Int)
  {
      println("X-GOOGLE-CALENDAR-CONTENT-TITLE" , title)
      println("X-GOOGLE-CALENDAR-CONTENT-ICON"  , icon)
      println("X-GOOGLE-CALENDAR-CONTENT-URL"   , url)
      println("X-GOOGLE-CALENDAR-CONTENT-TYPE"  , "text/html") // can be image/*
      println("X-GOOGLE-CALENDAR-CONTENT-WIDTH" , width.toString)
      println("X-GOOGLE-CALENDAR-CONTENT-HEIGHT", height.toString)
  }


  def endEvent = println("END", "VEVENT")


  private def println(name: String, value: String) {
    out.print(name)
    out.print(":")
    out.println(value)
  }

  private def println(line: String) {
    out.println(line)
  }
}
