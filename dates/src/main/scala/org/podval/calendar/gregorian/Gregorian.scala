package org.podval.calendar.gregorian

import org.podval.calendar.dates.{Calendar, CalendarMember}
import org.podval.calendar.numbers.{Digits, PointCompanion}

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def numbers: Gregorian = Gregorian.this
  }

  final override type Year = GregorianYear

  final override type YearCharacter = Boolean

  final override object Year extends GregorianYearCompanion with GregorianCalendarMember {
    def apply(number: Int): Year = new GregorianYear(number) with GregorianCalendarMember
  }

  final override type Month = GregorianMonth

  final override type MonthName = Month.Name

  final override object Month extends GregorianMonthCompanion with GregorianCalendarMember {
    override def apply(number: Int): Month = new GregorianMonth(number) with GregorianCalendarMember
  }

  final override type Day = GregorianDay

  final override type DayName = Day.Name

  final override object Day extends GregorianDayCompanion with GregorianCalendarMember {
    override def apply(number: Int): Day = new GregorianDay(number) with GregorianCalendarMember
  }

  final override type Point = GregorianMoment

  final override object Point extends PointCompanion[Gregorian] with GregorianCalendarMember {
    override def apply(digits: Int*): Point = new Digits(digits) with GregorianMoment {
      final override def companion: PointCompanion[Gregorian] = Point
    }
  }
}


object Gregorian extends Gregorian
