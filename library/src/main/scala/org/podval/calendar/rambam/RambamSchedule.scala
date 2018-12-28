package org.podval.calendar.rambam

import org.podval.calendar.jewish.Jewish.{Day, Month, Year}
import org.podval.judaica.rambam.MishnehTorah

/**
 * Generate Rambam study schedule for MishnehTorah and Sefer HaMitzvos
 */
object RambamSchedule {
  private val chapters: Seq[MishnehTorah.Chapter] = MishnehTorah.books.flatMap(_.parts.flatMap(_.chapters))

  private val numberOfChapters: Int = chapters.length

  final val numberOfLessons: Int = 339

  require(numberOfLessons*3 == numberOfChapters)

  final val epoch: Day = Year(5744).month(Month.Name.Nisan).day(27)

  final def cycleNumber3chapters(day: Day): Int = (day - epoch) / numberOfLessons + 1

  final def lessonNumber(day: Day): Int = (day - epoch) % numberOfLessons + 1

  final def cycleNumber1chapter(day: Day): Int = (day - epoch) / numberOfChapters + 1

  final def chapterNumber1chapter(day: Day): Int = (day - epoch) % numberOfChapters + 1

  def main(args: Array[String]): Unit = {
    println(chapters(1).names.doFind(org.podval.judaica.metadata.Language.Russian.toSpec).name)
    // println(lessonNumber(org.podval.calendar.jewish.Jewish.nowDay))
    // printSchedule(Formatter.narrow)(5777)
  }

  def printSchedule(formatter: Formatter)(numYear: Int): Unit =
    Schedule.scheduleYear(Year(numYear), (day: Day) => lessonNumber(day).toString, formatter).foreach(println)
}
