package org.opentorah.calendar.roman

object Julian extends Roman {

  override protected def yearFirstDayCorrection(yearNumber: Int): Int =
    (yearNumber - 1)/4

  override protected def isLeapYear(yearNumber: Int): Boolean =
    yearNumber % 4 == 0

  // days before Julian January 1, 1 CE
  override val epoch: Int = 1373426

//  lazy val yearLength: TimeVector = TimeVector.fromRational(
//    BigRational(365) +
//    BigRational(1, 4),
//    length = maxLength
//  )
}
