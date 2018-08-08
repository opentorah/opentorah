package org.podval.calendar.jewish

import Jewish.{Day, Month, Year}
import Month.Name._
import Day.Name._
import org.podval.calendar.dates.Calendar.fromJewish

// Sample of using the calendar code
object ScalaSample {

  def main(args: Array[String]): Unit = {
    val simchasTorah = Year(5778).month(Tishrei).day(23)
    val shabbosBreishis = simchasTorah.next.next(Shabbos)
    println(shabbosBreishis)
    println(fromJewish(shabbosBreishis))
  }
}
