package org.podval.calendar.jewish

import org.podval.calendar.calendar.DayCompanion

abstract class JewishDayCompanion extends DayCompanion[Jewish] {
  final val Name: JewishDayName.type = JewishDayName

  final override def names: Seq[Jewish.DayName] = Name.values

  // It seems that first day of the first year was Sunday; molad - BaHaRad.
  // Second year - friday; molad - 8 in the morning.
  final override val firstDayNumberInWeek: Int = 1
}
