package org.opentorah.dates

import org.opentorah.metadata.NamedCompanion
import org.opentorah.numbers.NumbersMember

/**
  *
  */
abstract class MonthCompanion[C <: Calendar[C]](calendar: C)
  extends NumbersMember[C](calendar) with CalendarMember[C]
{
  val Name: NamedCompanion

  final type Name = Name.Key

  final def apply(number: Int): C#Month = apply(None, number)

  private[opentorah] def yearNumber(monthNumber: Int): Int

  private[opentorah] final def withNumberInYear(year: C#Year, numberInYear: Int): C#Month = {
    require(0 < numberInYear && numberInYear <= year.lengthInMonths)
    apply(Some(year), year.firstMonthNumber + numberInYear - 1)
  }

  private[opentorah] def apply(yearOpt: Option[C#Year], number: Int): C#Month

  private[opentorah] def numberInYear(monthNumber: Int): Int
}
