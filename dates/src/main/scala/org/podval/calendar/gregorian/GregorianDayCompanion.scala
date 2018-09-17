package org.podval.calendar.gregorian

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.judaica.metadata.NamesLoader

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  final override val Name: GregorianDayCompanion.type = GregorianDayCompanion

  final override def names: Seq[Name] = GregorianDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekGregorian
}

object GregorianDayCompanion extends NamesLoader {
  sealed trait Key extends KeyBase

  case object Sunday extends Key
  case object Monday extends Key
  case object Tuesday extends Key
  case object Wednesday extends Key
  case object Thursday extends Key
  case object Friday extends Key
  case object Saturday extends Key

  override val values: Seq[Key] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

  override def resourceName: String = "GregorianDay"
}
