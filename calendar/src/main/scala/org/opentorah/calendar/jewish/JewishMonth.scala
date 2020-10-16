package org.opentorah.calendar.jewish

import org.opentorah.dates.MonthBase
import Jewish.Moment

final class JewishMonth private[jewish](calendar: Jewish, yearOpt: Option[Jewish#Year], number: Int)
  extends MonthBase[Jewish](calendar, yearOpt, number)
{
  def newMoon: Moment = Moon.newMoon(number)
}
