package org.podval.calendar.jewish

import org.podval.calendar.calendar.DayCompanion

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  override def names: Seq[Jewish#DayName] = Jewish.DayName.values

  // It seems that first day of the first year was Sunday; molad - BaHaRad.
  // Second year - friday; molad - 8 in the morning.
  override val firstDayNumberInWeek: Int = 1
}
