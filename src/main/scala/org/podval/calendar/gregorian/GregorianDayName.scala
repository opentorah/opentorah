package org.podval.calendar.gregorian

import org.podval.calendar.util.Named

sealed class GregorianDayName(name: String) extends Named(name)

object GregorianDayName {
  case object Sunday extends GregorianDayName("Sunday")
  case object Monday extends GregorianDayName("Monday")
  case object Tuesday extends GregorianDayName("Tuesday")
  case object Wednesday extends GregorianDayName("Wednesday")
  case object Thursday extends GregorianDayName("Thursday")
  case object Friday extends GregorianDayName("Friday")
  case object Saturday extends GregorianDayName("Saturday")

  val values: Seq[GregorianDayName] =
    Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)
}
