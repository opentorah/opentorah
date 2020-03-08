package org.podval.calendar.dates

import org.opentorah.metadata.NamedCompanion

/**
  *
  */
abstract class MonthCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  val Name: NamedCompanion

  final type Name = Name.Key

  final def apply(number: Int): C#Month = apply(None, number)

  private[dates] def yearNumber(monthNumber: Int): Int

  private[dates] final def withNumberInYear(year: C#Year, numberInYear: Int): C#Month = {
    require(0 < numberInYear && numberInYear <= year.lengthInMonths)
    apply(Some(year), year.firstMonthNumber + numberInYear - 1)
  }

  private[dates] def apply(yearOpt: Option[C#Year], number: Int): C#Month

  private[dates] def numberInYear(monthNumber: Int): Int
}
