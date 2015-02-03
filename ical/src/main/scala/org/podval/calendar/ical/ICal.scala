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

import org.podval.calendar.dates.Gregorian.Day

import java.io.PrintStream


object ICal {

  type Properties = Seq[(String, String)]


  def print(properties: Properties, out: PrintStream): Unit = properties.foreach { case (name, value) =>
    out.println(s"$name:$value")
  }


  def beginCalendar(prodId: String, name: Option[String], description: Option[String]): Properties = {
    Seq(
      "BEGIN"    -> "VCALENDAR",
      "PRODID"   -> prodId,
      "VERSION"  -> "2.0",
      "CALSCALE" -> "GREGORIAN",
      "METHOD"   -> "PUBLISH"
    ) ++
      name       .map("X-WR-CALNAME" -> _) ++
      description.map("X-WR-CALDESC" -> _)
  }


  val endCalendar: Properties = Seq("END" -> "VCALENDAR")


  def beginEvent(transparent: Boolean): Properties = Seq(
    // "CATEGORIES"    -> "Holidays",
    // "URL;VALUE=URI" -> "http://lwhjsdgfjhf",
    // "DTSTAMP"       -> "2061121T044202Z",
    // "UID"           -> "asdhjgd-wjks=-f",

    "BEGIN"  -> "VEVENT",
    "CLASS"  -> "PUBLIC",
    "STATUS" -> "CONFIRMED",
    "TRANSP" -> (if (transparent) "TRANSPARENT" else "OPAQUE")
  )


  val endEvent: Properties = Seq("END" -> "VEVENT")


  def event(transparent: Boolean, properties: Properties): Properties =
    beginEvent(transparent) ++ properties ++ endEvent


  def fullDayDuration(day: Day): Properties = {
    def toDate(day: Day): String = {
      def pad2digits(what: Int): String = (if (what < 10) "0" else "") + what

      val yyyy = day.year.number
      val mm = pad2digits(day.month.numberInYear)
      val dd = pad2digits(day.numberInMonth)

      s"${yyyy}${mm}${dd}"
    }

    Seq(
      "DTSTART;VALUE=DATE" -> toDate(day),
      /////    println("DTEND;VALUE=DATE", toString(day.next))
      "DURATION" -> "P1D"
    )
  }


  def summary(text: String): Properties = Seq("SUMMARY" -> text)


  def googleContent(
    title: String,
    icon: String,
    url: String,
    width: Int,
    height: Int) : Properties = Seq(
    "X-GOOGLE-CALENDAR-CONTENT-TITLE"  -> title,
    "X-GOOGLE-CALENDAR-CONTENT-ICON"   -> icon,
    "X-GOOGLE-CALENDAR-CONTENT-URL"    -> url,
    "X-GOOGLE-CALENDAR-CONTENT-TYPE"   -> "text/html", // can be image/*
    "X-GOOGLE-CALENDAR-CONTENT-WIDTH"  -> width.toString,
    "X-GOOGLE-CALENDAR-CONTENT-HEIGHT" -> height.toString
  )
}
