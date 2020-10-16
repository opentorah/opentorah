package org.opentorah.calendar.gregorian

import org.opentorah.metadata.LanguageSpec
import org.opentorah.dates.Calendar

class Gregorian private() extends Calendar[Gregorian] {

  final override type Year = GregorianYear

  final override type YearCharacter = Boolean

  final override lazy val Year = new GregorianYearCompanion(Gregorian.this) {
    protected override def newYear(number: Int): Year =
      new GregorianYear(Gregorian.this, number)
  }

  final override type Month = GregorianMonth

  final override lazy val Month = new GregorianMonthCompanion(Gregorian.this) {
    private[opentorah] override def apply(yearOpt: Option[Year], number: Int): Month =
      new GregorianMonth(Gregorian.this, yearOpt, number)
  }

  final override type Day = GregorianDay

  final override lazy val Day = new GregorianDayCompanion(Gregorian.this) {
    private[opentorah] override def apply(monthOpt: Option[Month], number: Int): Day =
      new GregorianDay(Gregorian.this, monthOpt, number)
  }

  final override type Point = GregorianMoment

  final override type PointCompanionType = GregorianMomentCompanion

  final override lazy val Point = new GregorianMomentCompanion(Gregorian.this) {
    protected override def newNumber(digits: Seq[Int]): Point =
      new GregorianMoment(Gregorian.this, digits) {
        final override def companion: PointCompanionType = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = number.toString
}


object Gregorian extends Gregorian
