package org.opentorah.calendar.gregorian

import org.opentorah.calendar.dates.MonthBase

abstract class GregorianMonth private[gregorian](yearOpt: Option[Gregorian#Year], number: Int)
  extends MonthBase[Gregorian](yearOpt, number)
