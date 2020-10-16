package org.opentorah.calendar.gregorian

import org.opentorah.dates.DayBase

final class GregorianDay private[gregorian](calendar: Gregorian, monthOpt: Option[Gregorian.Month], number: Int)
  extends DayBase[Gregorian](calendar, monthOpt, number)
