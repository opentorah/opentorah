package org.opentorah.calendar.roman

import java.util.{GregorianCalendar, Calendar as Cal}

object Gregorian extends Roman:

  override protected def yearFirstDayCorrection(yearNumber: Int): Int =
    (yearNumber - 1)/4 - (yearNumber - 1)/100 + (yearNumber - 1)/400

  override protected def isLeapYear(yearNumber: Int): Boolean =
    (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

  // days before Gregorian January 1, 1 CE (Monday)
  // (in Calendrical Calculations: -1373427)
  override def epoch: Int = 1373428

//  lazy val yearLength: TimeVector = TimeVector.fromRational(
//    BigRational(365) +
//      BigRational(1, 4) -
//      BigRational(1, 100) +
//      BigRational(1, 400),
//    length = maxLength
//  )

  def now: Gregorian.Moment =
    val value: GregorianCalendar = new GregorianCalendar
    Gregorian
      .Year(value.get(Cal.YEAR))
      .month(value.get(Cal.MONTH)+1)
      .day(value.get(Cal.DAY_OF_MONTH)).toMoment
      .hours(value.get(Cal.HOUR_OF_DAY))
      .minutes(value.get(Cal.MINUTE))
      .secondsAndMilliseconds(value.get(Cal.SECOND), value.get(Cal.MILLISECOND))
