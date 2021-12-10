package org.opentorah.calendar.jewish

import Jewish.{Moment, Month, TimeVector, Year}

// TODO fold into Epoch? or into Calendar?
object Moon:
  final def newMoon(number: Int): Moment = firstNewMoon + meanLunarPeriod*(number-1)

  // KH 6:3
  final lazy val meanLunarPeriod: TimeVector = TimeVector().days(29).hours(12).parts(793)

  // KH 6:8 Molad of the year of Creation (#1; Man was created on Rosh Hashono of the year #2);
  // BeHaRaD: it was on the second day of the *week* (Monday),
  // but on the *first* day of the Jewish calendar epoch (?),
  // so zero days elapsed from it.
  private final lazy val firstNewMoon: Moment = Moment().days(0).nightHours(5).parts(204)

  // TODO verify that first two moladim were visible on the day they happened!

  // KH 6:8
  final lazy val firstMoladNisan: Moment = Year(1).month(Month.Nisan).newMoon
