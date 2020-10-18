package org.opentorah.calendar.gregorian

import org.opentorah.metadata.LanguageSpec
import org.opentorah.dates.Calendar

trait Gregorian extends Calendar[Gregorian] {

  final override type Year = GregorianYear

  final override type YearCharacter = Boolean

  final override lazy val Year = new GregorianYearCompanion {
    override val numbers: Gregorian = Gregorian.this
    protected override def newYear(number: Int): Year =
      new GregorianYear(number) {
        override val numbers: Gregorian = Gregorian.this
      }
  }

  final override type Month = GregorianMonth

  final override lazy val Month = new GregorianMonthCompanion {
    override val numbers: Gregorian = Gregorian.this
    private[opentorah] override def apply(yearOpt: Option[Year], number: Int): Month =
      new GregorianMonth(yearOpt, number) {
        override val numbers: Gregorian = Gregorian.this
      }
  }

  final override type Day = GregorianDay

  final override lazy val Day = new GregorianDayCompanion {
    override val numbers: Gregorian = Gregorian.this
    private[opentorah] override def apply(monthOpt: Option[Month], number: Int): Day =
      new GregorianDay(monthOpt, number) {
        override val numbers: Gregorian = Gregorian.this
      }
  }

  final override type Point = GregorianMoment

  final override lazy val Point: GregorianMomentCompanion = new GregorianMomentCompanion {
    override val numbers: Gregorian = Gregorian.this
    protected override def newNumber(digits: Seq[Int]): Point =
      new GregorianMoment(digits) {
        override val numbers: Gregorian = Gregorian.this
        final override def companion: GregorianMomentCompanion = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = number.toString
}


object Gregorian extends Gregorian
