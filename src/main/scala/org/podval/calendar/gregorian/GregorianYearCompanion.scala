package org.podval.calendar.gregorian

import org.podval.calendar.calendar.YearCompanion

abstract class GregorianYearCompanion extends YearCompanion[Gregorian] {
  protected final override def characters: Seq[Gregorian#YearCharacter] =
    Seq(true, false)

  protected final override def monthNamesAndLengths(isLeap: Gregorian#YearCharacter):
    List[Gregorian#MonthNameAndLength] =
  {
    import Gregorian.Month.Name._
    List(
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
    if (calendar.Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear
}
