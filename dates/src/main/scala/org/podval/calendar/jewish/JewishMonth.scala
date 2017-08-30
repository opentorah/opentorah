package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthBase
import Jewish.{Moment, Month}

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) {
  final def newMoon: Moment = JewishMonthCompanion.newMoon(number)
}
