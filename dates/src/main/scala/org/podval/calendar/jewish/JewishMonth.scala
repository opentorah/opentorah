package org.podval.calendar.jewish

import org.podval.calendar.calendar.MonthBase
import Jewish.{Month, Moment}

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) {
  final def newMoon: Moment = Month.firstNewMoon + Month.meanLunarPeriod*(number-1)
}
