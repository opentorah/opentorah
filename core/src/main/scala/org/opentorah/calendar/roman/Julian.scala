package org.opentorah.calendar.roman

object Julian extends Roman:

  override protected def numberOfLeapYears(yearNumber: Int): Int =
    yearNumber/4

  // Note: works for non-positive yearNumber too
  override protected def isLeapYear(yearNumber: Int): Boolean =
    yearNumber % 4 == 0

  // days from Creation to Julian January 1, 1 CE
  override val epoch: Int = 1373426

//  lazy val yearLength: TimeVector = TimeVector.fromRational(
//    BigRational(Calendar.fullDaysInSolarYear) +
//    BigRational(1, 4),
//    length = maxLength
//  )
