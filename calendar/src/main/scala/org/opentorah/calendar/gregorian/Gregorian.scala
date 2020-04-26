package org.opentorah.calendar.gregorian

import org.opentorah.metadata.LanguageSpec
import org.opentorah.calendar.dates.{Calendar, CalendarMember}
import org.opentorah.calendar.numbers.Digits

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def numbers: Gregorian = Gregorian.this
  }

  final override type NumbersMemberType = GregorianCalendarMember

  final override type Year = GregorianYear

  final override type YearCharacter = Boolean

  final override object Year extends GregorianYearCompanion with GregorianCalendarMember {
    protected override def newYear(number: Int): Year =
      new GregorianYear(number) with GregorianCalendarMember
  }

  final override type Month = GregorianMonth

  final override object Month extends GregorianMonthCompanion with GregorianCalendarMember {
    private[calendar] override def apply(yearOpt: Option[Year], number: Int): Month =
      new GregorianMonth(yearOpt, number) with GregorianCalendarMember
  }

  final override type Day = GregorianDay

  final override object Day extends GregorianDayCompanion with GregorianCalendarMember {
    private[calendar] override def apply(monthOpt: Option[Month], number: Int): Day =
      new GregorianDay(monthOpt, number) with GregorianCalendarMember
  }

  final override type Point = GregorianMoment

  final override type PointCompanionType = GregorianMomentCompanion

  final override object Point extends GregorianMomentCompanion with NumbersMemberType {
    protected override def newNumber(digits: Seq[Int]): Point =
      new Digits(digits) with GregorianMoment with NumbersMemberType {
        final override def companion: PointCompanionType = Point
      }
  }

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = number.toString
}


object Gregorian extends Gregorian
