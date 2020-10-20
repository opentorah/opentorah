package org.opentorah.calendar.jewish

import org.opentorah.dates.MonthBase
import Jewish.Moment

trait JewishMonth extends MonthBase[Jewish] {
  def newMoon: Moment = Moon.newMoon(number)
}
