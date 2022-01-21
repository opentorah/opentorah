package org.opentorah.calendar.roman

import java.time.LocalDateTime
import java.time.temporal.ChronoField

object Gregorian extends Roman:

  override protected def numberOfLeapYears(yearNumber: Int): Int =
    yearNumber/4 - yearNumber/100 + yearNumber/400

  // Note: works for non-positive yearNumber too
  override protected def isLeapYear(yearNumber: Int): Boolean =
    (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

  // days from Creation to Gregorian January 1, 1 CE (Monday)
  // (in Calendrical Calculations: -1373427)
  override def epoch: Int = 1373428

//  lazy val yearLength: TimeVector = TimeVector.fromRational(
//    BigRational(Calendar.fullDaysInSolarYear) +
//      BigRational(1, 4) -
//      BigRational(1, 100) +
//      BigRational(1, 400),
//    length = maxLength
//  )

  // TODO move into Gregorian.MomentCompanion
  def fromLocalDateTime(value: LocalDateTime): Moment = Gregorian
    .Year(value.getYear)
    .month(value.getMonthValue-1)
    .day(value.getDayOfMonth-1)
    .toMoment
    .hours(value.getHour)
    .minutes(value.getMinute)
    .secondsAndMilliseconds(
      seconds = value.getSecond,
      milliseconds = value.get(ChronoField.MILLI_OF_SECOND)
    ) // TODO take care of nanoseconds


  // TODO move into Gregorian.Moment
  final def toLocalDateTime(moment: Gregorian.Moment): LocalDateTime =
    val day: Gregorian.Day = moment.day
    LocalDateTime.of(
      day.year.number,
      day.month.numberInYear+1,
      day.numberInMonth+1,
      moment.hours,
      moment.minutes,
      moment.seconds,
      moment.milliseconds*1000000
    ) // TODO take care of nanoseconds
