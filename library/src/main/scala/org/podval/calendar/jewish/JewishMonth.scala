package org.podval.calendar.jewish

import org.podval.calendar.dates.MonthBase
import Jewish.Moment

abstract class JewishMonth private[jewish](yearOpt: Option[Jewish#Year], number: Int)
  extends MonthBase[Jewish](yearOpt, number)
{
  final def newMoon: Moment = Moon.newMoon(number)
}
