package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthBase
import Jewish.Moment

abstract class JewishMonth(year: Jewish#Year, number: Int) extends MonthBase[Jewish](year, number) {
  final def newMoon: Moment = Moon.newMoon(number)
}
