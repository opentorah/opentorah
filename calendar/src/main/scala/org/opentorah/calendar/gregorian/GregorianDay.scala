package org.opentorah.calendar.gregorian

import org.opentorah.dates.DayBase

abstract class GregorianDay private[gregorian](monthOpt: Option[Gregorian.Month], number: Int)
  extends DayBase[Gregorian](monthOpt, number)
