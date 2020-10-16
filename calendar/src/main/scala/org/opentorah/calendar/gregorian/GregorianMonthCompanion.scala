package org.opentorah.calendar.gregorian

import org.opentorah.dates.MonthCompanion
import org.opentorah.metadata.{Named, NamedCompanion, Names}
import Gregorian.Year

abstract class GregorianMonthCompanion(calendar: Gregorian) extends MonthCompanion[Gregorian](calendar) {
  final override val Name: GregorianMonthCompanion.type = GregorianMonthCompanion

  private[opentorah] final override def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Year.monthsInYear + 1

  private[opentorah] final override def numberInYear(monthNumber: Int): Int =
    monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1
}

object GregorianMonthCompanion extends NamedCompanion {
  sealed trait Key extends Named {
    final override def names: Names = toNames(this)
  }

  case object January extends Key
  case object February extends Key
  case object March extends Key
  case object April extends Key
  case object May extends Key
  case object June extends Key
  case object July extends Key
  case object August extends Key
  case object September extends Key
  case object October extends Key
  case object November extends Key
  case object December extends Key

  override val values: Seq[Key] =
    Seq(January, February, March, April, May, June, July, August, September, October, November, December)

  protected override def resourceName: String = "GregorianMonth"
}
