package org.podval.calendar.gregorian

import org.podval.calendar.dates.MonthCompanion
import Gregorian.Year
import org.podval.calendar.metadata.{MetadataParser, Names, WithNames}

abstract class GregorianMonthCompanion extends MonthCompanion[Gregorian] {
  final val Name: GregorianMonthCompanion.type = GregorianMonthCompanion

  final type Name = Name.Name

  final override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Year.monthsInYear + 1

  final override def numberInYear(monthNumber: Int): Int =
    monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1
}


object GregorianMonthCompanion {
  sealed trait Name extends WithNames[Name] {
    def toNames: Map[Name, Names] = month2names
  }

  case object January extends Name
  case object February extends Name
  case object March extends Name
  case object April extends Name
  case object May extends Name
  case object June extends Name
  case object July extends Name
  case object August extends Name
  case object September extends Name
  case object October extends Name
  case object November extends Name
  case object December extends Name

  val values: Seq[Name] =
    Seq(January, February, March, April, May, June, July, August, September, October, November, December)

  private val month2names: Map[Name, Names] = MetadataParser.loadNames(this, "GregorianMonth", values)
}
