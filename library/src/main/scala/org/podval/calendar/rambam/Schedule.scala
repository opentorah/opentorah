package org.podval.calendar.rambam

import org.podval.calendar.dates.Calendar
import org.podval.calendar.jewish.Jewish.{Day, Month, Year}

object Schedule {
  def scheduleYear(year: Year, lesson: Day => String, formatter: Formatter): Iterator[String] = {
    def scheduleMonth(month: Month): Seq[String] = {
      val lessons = for {
        day <- month.days
        gDay = Calendar.fromJewish(day)
      } yield formatter.formatLesson(
        day.numberInMonth,
        gDay.month.numberInYear,
        gDay.numberInMonth,
        lesson(day)
      )

      val result = month.name.toString +: (if (lessons.size == 30) lessons else lessons ++ Seq(""))
      result.map(formatter.formatLine)
    }

    def scheduleMonths(months: Seq[Month]): Seq[String] = {
      def combine(what: Seq[String]): String =
        what.reduce((acc: String, r: String) => acc ++ "    " ++ r)
      val schedules: Seq[Seq[String]] = months.map(scheduleMonth)
      schedules.transpose.map(combine) ++ Seq("")
    }

    year.months.sliding(formatter.numColumns, formatter.numColumns).flatMap(scheduleMonths)
  }
}
