package org.podval.calendar.jewish

import org.podval.calendar.dates.YearBase
import Jewish.{Moment, Month, Year, YearCharacter}

abstract class JewishYear(number: Int) extends YearBase[Jewish](number) {
  require(0 < number)

  final def newMoon: Moment = month(1).newMoon

  final def newYearDelay: NewYear.Delay = newYearDelay(newMoon)

  private def newYearDelay(newMoon: Moment): NewYear.Delay = NewYear.delay(number, newMoon)

  final override def firstDayNumber: Int = {
    val nm = newMoon
    nm.dayNumber + newYearDelay(nm).days
  }

  final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  final override def character: YearCharacter = (isLeap, kind)

  final def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

  final def latestAdar: Month = month(if (isLeap) Month.Name.AdarII else Month.Name.Adar)

  final def isShemittah: Boolean = (number % 7) == 0
}
