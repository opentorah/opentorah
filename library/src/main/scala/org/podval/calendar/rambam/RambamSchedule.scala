package org.podval.calendar.rambam

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.judaica.rambam.MishnehTorah

/**
 * Rambam study schedule for MishnehTorah and Sefer HaMitzvos
 */
final class RambamSchedule(
  val threeChapters: RambamSchedule.ThreeChapters,
  val oneChapter: RambamSchedule.OneChapter
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

  private val chapters: Seq[MishnehTorah.Chapter] = MishnehTorah.books.flatMap(_.parts.flatMap(_.chapters))

  private val numberOfChapters: Int = chapters.length

  final val numberOfLessons: Int = 339

  require(numberOfLessons*3 == numberOfChapters)

  final val epoch: Day = Year(5744).month(Month.Name.Nisan).day(27)

  def forDay(day: Day): RambamSchedule = {
    val distance: Int = day - epoch
    new RambamSchedule(
      threeChapters = new ThreeChapters(
        cycle = distance / numberOfLessons + 1,
        lesson = distance % numberOfLessons + 1,
        chapter1 = chapters((distance % numberOfLessons)*3+0),
        chapter2 = chapters((distance % numberOfLessons)*3+1),
        chapter3 = chapters((distance % numberOfLessons)*3+2)
      ),
      oneChapter = new OneChapter(
        cycle = distance / numberOfChapters + 1,
        year = (distance % numberOfChapters) / numberOfLessons + 1,
        chapterNumber = distance % numberOfChapters + 1,
        chapter = chapters(distance % numberOfChapters)
      )
    )
  }

  def main(args: Array[String]): Unit = {
    println(chapters(1).names.doFind(org.podval.judaica.metadata.Language.Russian.toSpec).name)
    // println(lessonNumber(org.podval.calendar.jewish.Jewish.nowDay))
    // printSchedule(Formatter.narrow)(5777)
  }

  def printSchedule(formatter: Formatter)(numYear: Int): Unit = {
    def lessonNumber(day: Day): Int = (day - epoch) % numberOfLessons + 1
    Schedule.scheduleYear(Year(numYear), (day: Day) => lessonNumber(day).toString, formatter).foreach(println)
  }
}
