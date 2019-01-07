package org.podval.calendar.gregorian

import org.podval.calendar.dates.MonthBase

abstract class GregorianMonth private[gregorian](yearOpt: Option[Gregorian#Year], number: Int)
  extends MonthBase[Gregorian](yearOpt, number)
