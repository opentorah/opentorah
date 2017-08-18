package org.podval.calendar.gregorian

import org.podval.calendar.calendar.DayCompanion
import org.podval.calendar.jewish.Jewish

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  final val Name: GregorianDayName.type = GregorianDayName

  final override def names: Seq[Gregorian.DayName] = Name.values

  final val epoch: Int = 1373429

  final override val firstDayNumberInWeek: Int =
    (((Jewish.Day.firstDayNumberInWeek - 1) + (epoch % daysPerWeek)) % daysPerWeek) + 1
}
