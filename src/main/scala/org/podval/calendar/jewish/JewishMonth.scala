package org.podval.calendar.jewish

import org.podval.calendar.calendar.MonthBase

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) { this: Jewish#Month =>
  final def newMoon: Jewish#Moment =
    calendar.Month.firstNewMoon + calendar.Month.meanLunarPeriod*(number-1)
}
