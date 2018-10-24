package org.podval.calendar.jewish

import org.podval.calendar.dates.YearBase
import Jewish.{Year, Moment, Month, YearCharacter}

abstract class JewishYear(number: Int) extends YearBase[Jewish](number) {
  require(0 < number)

  final def newMoon: Moment = month(1).newMoon

  final def newYearDelay: NewYear.Delay = NewYear.delay(number, newMoon)

  final override def firstDayNumber: Int = newMoon.day.number + newYearDelay.days

  final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  final override def character: YearCharacter = (isLeap, kind)

  final def kind: Year.Kind = Year.kind(isLeap, lengthInDays)

  final def cycle: Int = Cycle.yearCycle(number)

  final def numberInCycle: Int = Cycle.yearNumberInCycle(number)

  final def latestAdar: Month = month(if (isLeap) Month.Name.AdarII else Month.Name.Adar)
}
