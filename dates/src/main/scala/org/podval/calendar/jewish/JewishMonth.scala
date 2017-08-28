package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthBase
import Jewish.{Moment, Month}

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) {
  final def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
}
