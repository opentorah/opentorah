package org.podval.calendar.gregorian

import org.podval.calendar.dates.{Calendar, DayCompanion}
import org.podval.calendar.util.Named

abstract class GregorianDayCompanion extends DayCompanion[Gregorian] {
  final type Name = GregorianDayCompanion.Name

  final val Name: GregorianDayCompanion.Name.type = GregorianDayCompanion.Name

  final override def names: Seq[Name] = Name.values

  final override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekGregorian
}


object GregorianDayCompanion {
  sealed class Name(name: String) extends Named(name)

  object Name {
    case object Sunday extends Name("Sunday")
    case object Monday extends Name("Monday")
    case object Tuesday extends Name("Tuesday")
    case object Wednesday extends Name("Wednesday")
    case object Thursday extends Name("Thursday")
    case object Friday extends Name("Friday")
    case object Saturday extends Name("Saturday")

    val values: Seq[Name] =
      Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
  }
}
