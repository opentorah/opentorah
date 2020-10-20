package org.opentorah.calendar.gregorian

import org.opentorah.dates.YearCompanion
import org.opentorah.numbers.BigRational
import Gregorian.{TimeVector, Year, YearCharacter}
import Gregorian.Month.Name._

trait GregorianYearCompanion extends YearCompanion[Gregorian] { this: YearCompanion[Gregorian] =>
  protected final override def characters: Seq[YearCharacter] =
    Seq(true, false)

  protected final override def monthNamesAndLengths(isLeap: YearCharacter): Seq[numbers.MonthNameAndLength] = {
    Seq(
      new numbers.MonthNameAndLength(January  , 31),
      new numbers.MonthNameAndLength(February , if (isLeap) 29 else 28),
      new numbers.MonthNameAndLength(March    , 31),
      new numbers.MonthNameAndLength(April    , 30),
      new numbers.MonthNameAndLength(May      , 31),
      new numbers.MonthNameAndLength(June     , 30),
      new numbers.MonthNameAndLength(July     , 31),
      new numbers.MonthNameAndLength(August   , 31),
      new numbers.MonthNameAndLength(September, 30),
      new numbers.MonthNameAndLength(October  , 31),
      new numbers.MonthNameAndLength(November , 30),
      new numbers.MonthNameAndLength(December , 31)
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
