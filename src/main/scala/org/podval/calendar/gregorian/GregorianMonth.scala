package org.podval.calendar.gregorian

import org.podval.calendar.calendar.MonthBase

abstract class GregorianMonth(number: Int) extends MonthBase[Gregorian](number) {
  this: Gregorian.Month =>
}
