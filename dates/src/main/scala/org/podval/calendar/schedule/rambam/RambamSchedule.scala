package org.podval.calendar.schedule.rambam

import org.podval.calendar.schedule.{Formatter, Schedule}
import org.podval.calendar.jewish.Jewish.{Year, Month, Day}

/**
 * Generate Rambam study schedule for Sefer HaMitzvos
 */
object RambamSchedule {
  final val numberOfLessons: Int = 339

  final val firstLessonDay: Day = Year(5775).month(Month.Name.Kislev).day(23)

  final def lessonNumber(day: Day): Int = {
    // % misbehaves on negatives :)
    val distance = day.number - firstLessonDay.number + 50 * numberOfLessons
    distance % numberOfLessons + 1
  }

  def printSchedule(formatter: Formatter)(numYear: Int): Unit =
    Schedule.scheduleYear(Year(numYear), (day: Day) => lessonNumber(day).toString, formatter).foreach(println)

  def main(args: Array[String]): Unit = printSchedule(Formatter.narrow)(5777)
}
