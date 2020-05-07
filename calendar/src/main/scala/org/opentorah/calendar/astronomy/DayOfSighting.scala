package org.opentorah.calendar.astronomy

import org.opentorah.angles.Angles.Rotation
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.calendar.jewish.Jewish.{Day, Moment, Month, Year}
import org.opentorah.numbers.Math

object DayOfSighting {
  val calculator: Calculator = Calculator.Text

  // Trying to figure out why Iyar 2 4938 is the day of sighting
  def elongation(moment: Moment): Rotation = calculator.calculate(moment.day).elongation

  def dayOfSighting(month: Month): Day = {
    val from: Moment = (month.firstDay - 5).toMoment
    val to: Moment = (month.firstDay + 5).toMoment
    val result: Moment = Math.findZero[Jewish, Rotation](
      elongation,
      from,
      to,
      length = 1)
    result.day
  }

  def main(args: Array[String]): Unit = {
    val from: Day = Year(4938).month(Month.Name.Iyar).firstDay - 5
    for (offset <- 0 to 10) {
      val day: Day = from + offset
      val e = elongation(day.toMoment)
      println(s"day: $day; elongation: $e")
    }
    //    println(Text.dayOfSighting(Year(4938).month(Month.Name.Iyar)))
  }
}
