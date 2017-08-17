package org.podval.calendar.dates

import org.podval.calendar.gregorian.Gregorian
import org.podval.calendar.jewish.Jewish

object Conversions {

  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  val jewishDayStartHours = 18

  val gregorianDayStartHours: Int = Jewish.numberSystem.hoursPerDay - jewishDayStartHours

  def toJewish(moment: Gregorian#Moment): Jewish#Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours >= jewishDayStartHours)
        (moment.day.next, hours - jewishDayStartHours) else
        (moment.day     , hours + gregorianDayStartHours)

    toJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  def fromJewish(moment: Jewish#Moment): Gregorian#Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours < gregorianDayStartHours)
        (moment.day.prev, hours + jewishDayStartHours) else
        (moment.day     , hours - gregorianDayStartHours)

    fromJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  def fromJewish(day: Jewish   #Day): Gregorian#Day = Gregorian.Day(day.number - Gregorian.Day.epoch)

  def toJewish  (day: Gregorian#Day): Jewish   .Day = Jewish   .Day(day.number + Gregorian.Day.epoch)
}
