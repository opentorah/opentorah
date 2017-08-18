package org.podval.calendar.gregorian

import org.podval.calendar.calendar.MonthCompanion

abstract class GregorianMonthCompanion extends MonthCompanion[Gregorian] {
  val Name: GregorianMonthName.type = GregorianMonthName

  final override def yearNumber(monthNumber: Int): Int =
    (monthNumber - 1) / Gregorian.Year.monthsInYear + 1

  final override def numberInYear(monthNumber: Int): Int =
    monthNumber - Gregorian.Year.firstMonth(yearNumber(monthNumber)) + 1
}
