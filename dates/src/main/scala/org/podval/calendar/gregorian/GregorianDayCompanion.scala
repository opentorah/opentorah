package org.podval.calendar.gregorian

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.calendar.metadata.{MetadataParser, Names, WithNames}

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  final val Name: GregorianDayCompanion.type = GregorianDayCompanion

  final type Name = Name.Name // TODO push into DayCompanion

  final override def names: Seq[Name] = GregorianDayCompanion.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekGregorian
}


object GregorianDayCompanion {
  sealed trait Name extends WithNames[Name] {
    def toNames: Map[Name, Names] = day2names
  }

  case object Sunday extends Name
  case object Monday extends Name
  case object Tuesday extends Name
  case object Wednesday extends Name
  case object Thursday extends Name
  case object Friday extends Name
  case object Saturday extends Name

  val values: Seq[Name] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

  private val day2names: Map[Name, Names] = MetadataParser.loadNames(this, "GregorianDay", values)
}
