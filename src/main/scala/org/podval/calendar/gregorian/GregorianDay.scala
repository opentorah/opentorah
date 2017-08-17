package org.podval.calendar.gregorian

import org.podval.calendar.calendar.DayBase

abstract class GregorianDay(number: Int) extends DayBase[Gregorian](number) {
  this: Gregorian#Day =>
}
