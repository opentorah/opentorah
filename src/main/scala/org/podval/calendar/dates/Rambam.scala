/*
 * Copyright 2015 Podval Group.
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


  def print(yearNumber: Int): Unit = {
    Year(yearNumber).days.foreach { day =>
      val month = day.month

      val gDay = Conversions.fromJewish(day)
      val gMonth = gDay.month

      val lesson = lessonForDay(day)
      val lessonString = f"${month.name}%-12s ${day.numberInMonth}%2d (${gMonth.numberInYear}%2d/${gDay.numberInMonth}%2d) $lesson%3d"
      println(lessonString)
    }
  }


  def main(args: Array[String]): Unit = {
    print(5776)
  }
}
