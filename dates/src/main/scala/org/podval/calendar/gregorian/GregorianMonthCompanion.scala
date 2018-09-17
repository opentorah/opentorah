package org.podval.calendar.gregorian

import org.podval.calendar.dates.MonthCompanion
import Gregorian.Year
import org.podval.calendar.metadata.NamesLoader

abstract class GregorianMonthCompanion extends MonthCompanion[Gregorian] {
  final val Name: GregorianMonthCompanion.type = GregorianMonthCompanion

  final type Name = GregorianMonthCompanion.GregorianMonthName // TODO push into MonthCompanion

  final override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Year.monthsInYear + 1

  final override def numberInYear(monthNumber: Int): Int =
    monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1
}

object GregorianMonthCompanion extends NamesLoader {
  sealed trait GregorianMonthName extends KeyBase

  override type Key = GregorianMonthName

  case object January extends GregorianMonthName
  case object February extends GregorianMonthName
  case object March extends GregorianMonthName
  case object April extends GregorianMonthName
  case object May extends GregorianMonthName
  case object June extends GregorianMonthName
  case object July extends GregorianMonthName
  case object August extends GregorianMonthName
  case object September extends GregorianMonthName
  case object October extends GregorianMonthName
  case object November extends GregorianMonthName
  case object December extends GregorianMonthName

  override val values: Seq[GregorianMonthName] =
    Seq(January, February, March, April, May, June, July, August, September, October, November, December)

  override def resourceName: String = "GregorianMonth"
}
