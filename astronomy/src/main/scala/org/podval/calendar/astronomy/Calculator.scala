package org.podval.calendar.astronomy

import org.podval.calendar.angles.Angles
import org.podval.calendar.jewish.Jewish
import Jewish.{Year, Day, Month, Moment}
import org.podval.calendar.numbers.Math

class Calculator(val epoch: Epoch, val calculators: Calculators, val rounders: Rounders) {

  def calculate(day: Day): Calculation = new Calculation(
    calculator = this,
    day = day
  )

  // TODO trying to figure out why Iyar 2 4938 is the day of sighting
  // TODO elongation should be a Rotation; eliminate the use of toPoint()
  def elongation(moment: Moment): Angles.Point = calculate(moment.day).elongation.toPoint.symmetrical

  def dayOfSighting(month: Month): Day = {
    val from: Moment = (month.firstDay - 5).toMoment
    val to: Moment = (month.firstDay + 5).toMoment
    val result = Math.findZero[Jewish, Angles](
      elongation,
      from,
      to,
      length = 1)
    result.day
  }
}


object Calculator {

  object Text extends Calculator(Epoch.Text, Calculators.Text, Rounders.Text)

  def main(args: Array[String]): Unit = {
    val from: Day = Year(4938).month(Month.Name.Iyar).firstDay - 5
    for (offset <- 0 to 10) {
      val day: Day = from + offset
      val elongation = Text.elongation(day.toMoment)
      println(s"day: $day; elongation: $elongation")
    }
//    println(Text.dayOfSighting(Year(4938).month(Month.Name.Iyar)))
  }
}
