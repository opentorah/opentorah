package org.podval.calendar.gregorian

import org.podval.calendar.dates.DayBase

abstract class GregorianDay private[gregorian](monthOpt: Option[Gregorian.Month], number: Int)
  extends DayBase[Gregorian](monthOpt, number)
