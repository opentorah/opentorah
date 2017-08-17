package org.podval.calendar.gregorian

import org.podval.calendar.calendar.DayCompanion
import org.podval.calendar.jewish.Jewish

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  override def names: Seq[Gregorian#DayName] = GregorianDayName.values

  override def apply(number: Int): Gregorian#Day = calendar.createDay(number)

  val epoch: Int = 1373429

  override val firstDayNumberInWeek: Int =
    (((Jewish.Day.firstDayNumberInWeek - 1) + (epoch % daysPerWeek)) % daysPerWeek) + 1
}
