package org.podval.calendar.jewish

import Jewish.{Year, Month, Day}

object RambamSchedule {
  final val numberOfLessons: Int = 339

  final val firstLessonDay: Day = Year(5775).month(Month.Name.Kislev).day(23)

  final def lessonNumber(day: Day): Int = {
    // % misbehaves on negatives :)
    val distance = day.number - firstLessonDay.number + 50 * numberOfLessons
    distance % numberOfLessons + 1
  }
}
