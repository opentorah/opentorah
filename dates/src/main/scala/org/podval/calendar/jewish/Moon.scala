package org.podval.calendar.jewish

import org.podval.calendar.jewish.Jewish.{Moment, TimeInterval}

object Moon {
  // TODO move; merge the rest with Sun into Epoch(s) (and base Seasons on it)
  final def newMoon(number: Int): Moment = firstNewMoon + meanLunarPeriod*(number-1)

  // KH 6:3
  final val meanLunarPeriod: TimeInterval = TimeInterval().days(29).hours(12).parts(793)

  // KH 6:8 Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2), BeHaRaD:
  final val firstNewMoon: Moment = Moment().day(2).nightHours(5).parts(204)
}
