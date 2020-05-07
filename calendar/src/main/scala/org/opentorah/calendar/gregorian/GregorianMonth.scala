package org.opentorah.calendar.gregorian

import org.opentorah.dates.MonthBase

abstract class GregorianMonth private[gregorian](yearOpt: Option[Gregorian#Year], number: Int)
  extends MonthBase[Gregorian](yearOpt, number)
