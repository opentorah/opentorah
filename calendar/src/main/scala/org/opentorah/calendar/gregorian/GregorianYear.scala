package org.opentorah.calendar.gregorian

import org.opentorah.dates.YearBase
import Gregorian.{Year, YearCharacter}

final class GregorianYear(calendar: Gregorian, number: Int) extends YearBase[Gregorian](calendar, number) {
  override def firstDayNumber: Int = Year.firstDay(number)

  override def lengthInDays: Int = Year.lengthInDays(number)

  override def character: YearCharacter = isLeap
}
