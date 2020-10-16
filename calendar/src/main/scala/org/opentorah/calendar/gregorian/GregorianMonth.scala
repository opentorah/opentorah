package org.opentorah.calendar.gregorian

import org.opentorah.dates.MonthBase

final class GregorianMonth private[gregorian](calendar: Gregorian, yearOpt: Option[Gregorian#Year], number: Int)
  extends MonthBase[Gregorian](calendar, yearOpt, number)
