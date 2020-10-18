package org.opentorah.calendar.gregorian

import org.opentorah.dates.YearCompanion
import org.opentorah.numbers.BigRational
import Gregorian.{MonthNameAndLength, TimeVector, Year, YearCharacter}
import Gregorian.Month.Name._

trait GregorianYearCompanion extends YearCompanion[Gregorian] { this: YearCompanion[Gregorian] =>
  protected final override def characters: Seq[YearCharacter] =
    Seq(true, false)

  protected final override def monthNamesAndLengths(isLeap: YearCharacter):
    Seq[MonthNameAndLength] =
  {
    Seq(
      createMonthNameAndLength(January  , 31),
      createMonthNameAndLength(February , if (isLeap) 29 else 28),
      createMonthNameAndLength(March    , 31),
      createMonthNameAndLength(April    , 30),
      createMonthNameAndLength(May      , 31),
      createMonthNameAndLength(June     , 30),
      createMonthNameAndLength(July     , 31),
      createMonthNameAndLength(August   , 31),
      createMonthNameAndLength(September, 30),
      createMonthNameAndLength(October  , 31),
      createMonthNameAndLength(November , 30),
      createMonthNameAndLength(December , 31)
    )
  }

  protected final override def areYearsPositive: Boolean = false

  final override def isLeap(yearNumber: Int): Boolean =
    (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

  final override def firstMonth(yearNumber: Int): Int =
    monthsInYear*(yearNumber - 1) + 1

  final override def lengthInMonths(yearNumber: Int): Int = monthsInYear

  final val monthsInYear: Int = 12

  private val daysInNonLeapYear: Int = 365

  final def firstDay(yearNumber: Int): Int =
    daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 +
      (yearNumber - 1)/400 + 1

  final def lengthInDays(yearNumber: Int): Int =
    if (Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear

  final lazy val yearLength: TimeVector = TimeVector.fromRational(
    BigRational(365) +
    BigRational(1, 4) -
    BigRational(1, 100) +
    BigRational(1, 400),
    length = numbers.maxLength
  )
}
