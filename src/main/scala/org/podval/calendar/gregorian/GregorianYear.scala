package org.podval.calendar.gregorian

import org.podval.calendar.calendar.YearBase

abstract class GregorianYear(number: Int)
  extends YearBase[Gregorian](number)
{ this: Gregorian.Year =>
  final override def firstDayNumber: Int = calendar.Year.firstDay(number)

  final override def lengthInDays: Int = calendar.Year.lengthInDays(number)

  final override def character: Gregorian.YearCharacter = isLeap
}
