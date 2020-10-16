package org.opentorah.calendar.jewish

import org.opentorah.dates.YearBase
import Jewish.{Moment, Month, Year, YearCharacter}

final class JewishYear(calendar: Jewish, number: Int) extends YearBase[Jewish](calendar, number) {
  require(0 <= number)

  def newMoon: Moment = month(1).newMoon

  def newYearDelay: NewYear.Delay = newYearDelay(newMoon)

  private def newYearDelay(newMoon: Moment): NewYear.Delay = NewYear.delay(number, newMoon)

  override def firstDayNumber: Int = {
    val nm = newMoon
    nm.dayNumber + newYearDelay(nm).days
  }

  override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  override def character: YearCharacter = (isLeap, kind)

  def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

  def latestAdar: Month = month(if (isLeap) Month.Name.AdarII else Month.Name.Adar)

  def isShemittah: Boolean = (number % 7) == 0
}
