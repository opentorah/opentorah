package org.podval.calendar.gregorian

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.calendar.metadata.{WithNames, WithNamesCompanion}

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  final val Name: GregorianDayCompanion.type = GregorianDayCompanion

  final type Name = GregorianDayName // TODO push into DayCompanion

  final override def names: Seq[Name] = GregorianDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekGregorian
}

sealed trait GregorianDayName extends WithNames[GregorianDayName] {
  def companion: WithNamesCompanion[GregorianDayName] = GregorianDayCompanion
}

object GregorianDayCompanion extends WithNamesCompanion[GregorianDayName] {
  case object Sunday extends GregorianDayName
  case object Monday extends GregorianDayName
  case object Tuesday extends GregorianDayName
  case object Wednesday extends GregorianDayName
  case object Thursday extends GregorianDayName
  case object Friday extends GregorianDayName
  case object Saturday extends GregorianDayName

  override val values: Seq[GregorianDayName] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

  override def resourceName: String = "GregorianDay"
}
