package org.opentorah.calendar.gregorian

import org.opentorah.dates.YearBase
import Gregorian.{Year, YearCharacter}

abstract class GregorianYear(number: Int)
  extends YearBase[Gregorian](number)
{
  final override def firstDayNumber: Int = Year.firstDay(number)

  final override def lengthInDays: Int = Year.lengthInDays(number)

  final override def character: YearCharacter = isLeap
}
