package org.opentorah.schedule.rambam

import org.opentorah.calendar.Calendar
import org.opentorah.calendar.jewish.Jewish.{Day, Month, Year}
import org.opentorah.calendar.roman.Gregorian
import org.opentorah.texts.rambam.{MishnehTorah, SeferHamitzvosLessons}

/**
 * Rambam study schedule for MishnehTorah and Sefer HaMitzvos
 */
final class RambamSchedule(
  val threeChapters: RambamSchedule.ThreeChapters,
  val oneChapter: RambamSchedule.OneChapter,
  val seferHamitzvos: SeferHamitzvosLessons.Lesson
)

object RambamSchedule {
  final class ThreeChapters(
    val cycle: Int,
    val lesson: Int,
    val chapter1: MishnehTorah.Chapter,
    val chapter2: MishnehTorah.Chapter,
    val chapter3: MishnehTorah.Chapter
  )

  final class OneChapter(
    val cycle: Int,
    val year: Int,
    val chapterNumber: Int,
    val chapter: MishnehTorah.Chapter
  )

  final val numberOfLessons: Int = 339
  require(SeferHamitzvosLessons.lessons.map(_.number) == (1 to numberOfLessons))

  private val chapters: Seq[MishnehTorah.Chapter] = MishnehTorah.books.flatMap(_.parts.flatMap(_.chapters))
  private val numberOfChapters: Int = chapters.length
  require(numberOfLessons*3 == numberOfChapters)

  final val epoch: Day = Year(5744).month(Month.Nisan).day(27)

  def forDay(day: Day): RambamSchedule = {
    val distance: Int = day - epoch
    val lesson: Int = Math.floorMod(distance, numberOfLessons)
    val chapter: Int = Math.floorMod(distance, numberOfChapters)
    new RambamSchedule(
      threeChapters = new ThreeChapters(
        cycle = distance / numberOfLessons + 1,
        lesson = lesson + 1,
        chapter1 = chapters(lesson*3+0),
        chapter2 = chapters(lesson*3+1),
        chapter3 = chapters(lesson*3+2)
      ),
      oneChapter = new OneChapter(
        cycle = distance / numberOfChapters + 1,
        year = chapter / numberOfLessons + 1,
        chapterNumber = chapter + 1,
        chapter = chapters(chapter)
      ),
      seferHamitzvos = SeferHamitzvosLessons.lessons(lesson)
    )
  }

  def main(args: Array[String]): Unit =
    scheduleYear(Year(5777), Formatter.narrow).foreach(println)

  def scheduleYear(year: Year, formatter: Formatter): Iterator[String] = {
    def lesson(day: Day): String = ((day - epoch) % numberOfLessons + 1).toString

    def scheduleMonth(month: Month): Seq[String] = {
      val lessons = for {
        day <- month.days
        gDay = Gregorian.Day.from(day.asInstanceOf[Calendar#Day])
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
