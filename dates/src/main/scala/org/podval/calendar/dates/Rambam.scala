package org.podval.calendar.dates

import org.podval.calendar.dates.jewish.Jewish.{Year, Month, Day}


/**
 * Generate Rambam study schedule for Sefer HaMitzvos
 */
object Rambam {

  val numberOfLessons: Int = 339

  val firstLessonDay: Int = Year(5775).month(Month.Name.Kislev).day(23).number

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
    override def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: Int
    ): String = f"$dayNumberInMonth%2d ($gMonthNumber%2d/$gDayNumberInMonth%2d) $lesson%3d"

    override def formatLine(line: String): String = f"$line%-14s"

    override def numColumns: Int = 4
  }


  private val narrowFormatter = new Formatter {
    override def formatLesson(
      dayNumberInMonth: Int,
      gMonthNumber: Int,
      gDayNumberInMonth: Int,
      lesson: Int
    ): String = f"$dayNumberInMonth%2d $lesson%3d"

    override def formatLine(line: String): String = f"${line.take(6)}%-6s"

    override def numColumns: Int = 8
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
      def combine(what: Seq[String]): String =
        what.reduce((acc: String, r: String) => acc ++ "    " ++ r)
      val schedules: Seq[Seq[String]] = months.map(scheduleMonth)
      schedules.transpose.map(combine) ++ Seq("")
    }

    year.months.sliding(formatter.numColumns, formatter.numColumns).map(scheduleMonths).flatten
  }

  def printSchedule(formatter: Formatter)(numYear: Int): Unit =
    scheduleYear(formatter, Year(numYear)).foreach(println)

  def main(args: Array[String]): Unit = printSchedule(narrowFormatter)(5777)
}
