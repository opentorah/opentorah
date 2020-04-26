package org.opentorah.calendar.jewish

import org.opentorah.calendar.jewish.Jewish.{Moment, Month, TimeVector, Year}

object Moon {
  final def newMoon(number: Int): Moment = firstNewMoon + meanLunarPeriod*(number-1)

  // KH 6:3
  final val meanLunarPeriod: TimeVector = TimeVector().days(29).hours(12).parts(793)

  // KH 6:8 Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2), BeHaRaD:
  final val firstNewMoon: Moment = Moment().day(2).nightHours(5).parts(204)

  // KH 6:8
  final lazy val firstMoladNisan: Moment = Year(1).month(Month.Name.Nisan).newMoon
}
