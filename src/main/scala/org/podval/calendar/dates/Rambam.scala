/*
 * Copyright 2015-2016 Podval Group.
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

import Jewish._


/**
 * Generate Rambam study schedule for Sefer HaMitzvos
 */
object Rambam {

  val numberOfLessons = 339


  val firstLessonDay = Year(5775).month(Month.Kislev).day(23).number


  def lessonForDay(day: Day): Int = {
    val distance = day.number - firstLessonDay + 50*numberOfLessons // % misbehaves on negatives :)
    distance % numberOfLessons + 1
  }

  private trait Formatter {

    def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: Int
    ): String


    def formatLine(line: String): String


    def numColumns: Int
  }

  private val wideFormatter = new Formatter {
    def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: Int
    ): String = f"$dayNumberInMonth%2d ($gMonthNumber%2d/$gDayNumberInMonth%2d) $lesson%3d"


    def formatLine(line: String): String = f"$line%-14s"


    def numColumns: Int = 4
  }


  private val narrowFormatter = new Formatter {
    def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: Int
    ): String = f"$dayNumberInMonth%2d $lesson%3d"


    def formatLine(line: String): String = f"${line.take(6)}%-6s"


    def numColumns: Int = 8
  }


  def scheduleYear(formatter: Formatter, year: Year): Iterator[String] = {
    def scheduleMonth(month: Month): Seq[String] = {
      val lessons = for {
        day <- month.days
        gDay = Conversions.fromJewish(day)
      } yield formatter.formatLesson(
        day.numberInMonth,
        gDay.month.numberInYear,
        gDay.numberInMonth,
        lessonForDay(day)
      )

      val result = month.name.toString +: (if (lessons.size == 30) lessons else lessons ++ Seq(""))
      result.map(formatter.formatLine)
    }

    def scheduleMonths(months: Seq[Month]): Seq[String] = {
      def combine(what: Seq[String]): String = what.reduce((acc: String, r: String) => acc ++ "    " ++ r)
      val schedules: Seq[Seq[String]] = months.map(scheduleMonth)
      schedules.transpose.map(combine) ++ Seq("")
    }

    year.months.sliding(formatter.numColumns, formatter.numColumns).map(scheduleMonths).flatten
  }

  def printSchedule(formatter: Formatter)(numYear: Int): Unit = {
    scheduleYear(formatter, Year(numYear)).foreach(println(_))
  }


  def main(args: Array[String]): Unit = printSchedule(narrowFormatter)(5776)
}
