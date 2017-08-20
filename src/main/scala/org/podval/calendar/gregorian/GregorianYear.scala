package org.podval.calendar.gregorian

import org.podval.calendar.calendar.YearBase
import Gregorian.{Year, YearCharacter}

abstract class GregorianYear(number: Int)
  extends YearBase[Gregorian](number)
{
  final override def firstDayNumber: Int = Year.firstDay(number)

  final override def lengthInDays: Int = Year.lengthInDays(number)

  final override def character: YearCharacter = isLeap
}
