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
    private[opentorah] override def apply(yearOption: Option[Year], monthNumber: Int): Month =
      new GregorianMonth {
        override val numbers: Gregorian = Gregorian.this
        override protected var yearOpt: Option[Year] = yearOption
        override def number: Int = monthNumber
      }
  }

  final override type Day = GregorianDay

  final override lazy val Day = new GregorianDayCompanion {
    override val numbers: Gregorian = Gregorian.this
    private[opentorah] override def apply(monthOption: Option[Month], dayNumber: Int): Day =
      new GregorianDay {
        override val numbers: Gregorian = Gregorian.this
        override protected var monthOpt: Option[Month] = monthOption
        override def number: Int = dayNumber
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
