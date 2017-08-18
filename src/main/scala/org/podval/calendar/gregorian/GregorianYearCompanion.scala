package org.podval.calendar.gregorian

import org.podval.calendar.calendar.YearCompanion

abstract class GregorianYearCompanion extends YearCompanion[Gregorian] {
  protected final override def characters: Seq[Gregorian#YearCharacter] =
    Seq(true, false)

  protected final override def monthNamesAndLengths(isLeap: Gregorian#YearCharacter):
    List[Gregorian#MonthNameAndLength] =
  {
    def create(name: Gregorian#MonthName, length: Int): Gregorian#MonthNameAndLength =
      calendar.createMonthNameAndLength(name, length)

    import Gregorian.Month.Name._
    List(
      create(January  , 31),
      create(February , if (isLeap) 29 else 28),
      create(March    , 31),
      create(April    , 30),
      create(May      , 31),
      create(June     , 30),
      create(July     , 31),
      create(August   , 31),
      create(September, 30),
      create(October  , 31),
      create(November , 30),
      create(December , 31)
    )
  }

  protected final override def areYearsPositive: Boolean = false

  final override def isLeap(yearNumber: Int): Boolean =
    (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

  final override def firstMonth(yearNumber: Int): Int =
    monthsInYear*(yearNumber - 1) + 1

  final override def lengthInMonths(yearNumber: Int): Int = monthsInYear

  val monthsInYear: Int = 12

  private val daysInNonLeapYear: Int = 365

  final def firstDay(yearNumber: Int): Int =
    daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 +
      (yearNumber - 1)/400 + 1

  final def lengthInDays(yearNumber: Int): Int =
    if (calendar.Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear
}
