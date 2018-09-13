package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthBase
import Jewish.Moment

abstract class JewishMonth(number: Int) extends MonthBase[Jewish](number) {
  final def newMoon: Moment = Moon.newMoon(number)
}
