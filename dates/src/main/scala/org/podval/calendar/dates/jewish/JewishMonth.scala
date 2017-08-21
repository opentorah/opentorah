package org.podval.calendar.dates.jewish

import org.podval.calendar.dates.calendar.MonthBase
import Jewish.{Month, Moment}

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) {
  final def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
}
