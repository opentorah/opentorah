package org.opentorah.calendar.gregorian

import org.opentorah.dates.YearBase
import Gregorian.{Year, YearCharacter}

abstract class GregorianYear(number: Int) extends YearBase[Gregorian](number) {
  override def firstDayNumber: Int = Year.firstDay(number)

  override def lengthInDays: Int = Year.lengthInDays(number)

  override def character: YearCharacter = isLeap
}
