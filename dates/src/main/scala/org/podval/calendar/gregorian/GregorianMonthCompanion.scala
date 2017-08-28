package org.podval.calendar.gregorian

import org.podval.calendar.dates.MonthCompanion
import org.podval.calendar.util.Named
import Gregorian.Year

abstract class GregorianMonthCompanion extends MonthCompanion[Gregorian] {
  final type Name = GregorianMonthCompanion.Name

  final val Name: GregorianMonthCompanion.Name.type = GregorianMonthCompanion.Name

  final override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Year.monthsInYear + 1

  final override def numberInYear(monthNumber: Int): Int =
    monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1
}


object GregorianMonthCompanion {
  sealed class Name(name: String) extends Named(name)

  object Name {
    case object January extends Name("January")
    case object February extends Name("February")
    case object March extends Name("March")
    case object April extends Name("April")
    case object May extends Name("May")
    case object June extends Name("June")
    case object July extends Name("July")
    case object August extends Name("August")
    case object September extends Name("September")
    case object October extends Name("October")
    case object November extends Name("November")
    case object December extends Name("December")
  }
}
