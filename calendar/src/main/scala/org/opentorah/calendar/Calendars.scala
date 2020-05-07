package org.opentorah.calendar

import java.util.{GregorianCalendar, Calendar => Cal}
import org.opentorah.calendar.gregorian.Gregorian
import org.opentorah.calendar.dates.{Calendar, MomentBase}
import org.opentorah.calendar.jewish.Jewish
import org.opentorah.times.Times

object Calendars {

  def now(calendar: Calendar[_]): MomentBase[_] = calendar match {
    case _: Jewish    => Calendars.nowJewish
    case _: Gregorian => Calendars.nowGregorian
  }

  final def nowJewish: Jewish#Moment = toJewish(now)

  final def nowGregorian: Gregorian#Moment = toGregorian(now)

  private def now: GregorianCalendar = new GregorianCalendar

  final def toJewish(value: GregorianCalendar): Jewish#Moment =
    toJewish(toGregorian(value))

  final def toGregorian(value: GregorianCalendar): Gregorian#Moment = Gregorian
    .Year(value.get(Cal.YEAR))
    .month(value.get(Cal.MONTH)+1)
    .day(value.get(Cal.DAY_OF_MONTH)).toMoment
    .hours(value.get(Cal.HOUR_OF_DAY))
    .minutes(value.get(Cal.MINUTE))
    .secondsAndMilliseconds(value.get(Cal.SECOND), value.get(Cal.MILLISECOND))

  //  Jewish  :   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Georgian:  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  private final val dayStartHoursJewish = 18

  private final val dayStartHoursGregorian: Int = Times.hoursPerDay - dayStartHoursJewish

  final def toJewish(moment: Gregorian.Moment): Jewish.Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours >= dayStartHoursJewish)
        (moment.day.next, hours - dayStartHoursJewish) else
        (moment.day     , hours + dayStartHoursGregorian)

    toJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  final def fromJewish(moment: Jewish.Moment): Gregorian.Moment = {
    val hours = moment.hours

    val (newDay, newHours) =
      if (hours < dayStartHoursGregorian)
        (moment.day.prev, hours + dayStartHoursJewish) else
        (moment.day     , hours - dayStartHoursGregorian)

    fromJewish(newDay).toMoment.hours(newHours).parts(moment.parts)
  }

  final def fromJewish(day: Jewish   .Day): Gregorian.Day = Gregorian.Day(day.number - Calendar.epoch)

  final def toJewish  (day: Gregorian.Day): Jewish   .Day = Jewish   .Day(day.number + Calendar.epoch)
}
