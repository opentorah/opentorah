package org.podval.calendar.gregorian

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def calendar: Gregorian = Gregorian.this
  }

  final override type Year = GregorianYear

  final override def createYear(number: Int): Gregorian#Year =
    new GregorianYear(number) with GregorianCalendarMember

  final override type YearCharacter = Boolean

  final override val Year: GregorianYearCompanion =
    new GregorianYearCompanion with GregorianCalendarMember

  final override type Month = GregorianMonth

  final override def createMonth(number: Int): Gregorian#Month =
    new GregorianMonth(number) with GregorianCalendarMember

  final override type MonthName = GregorianMonthName

  // TODO stick it into the Month companion???
  val MonthName: GregorianMonthName.type = GregorianMonthName

  final override val Month: GregorianMonthCompanion =
    new GregorianMonthCompanion with GregorianCalendarMember

  final override type Day = GregorianDay

  final override def createDay(number: Int): Gregorian#Day =
    new GregorianDay(number) with GregorianCalendarMember

  final override type DayName = GregorianDayName

  // TODO stick it into the Day companion???
  final val DayName: GregorianDayName.type = GregorianDayName

  final override val Day: GregorianDayCompanion =
    new GregorianDayCompanion with GregorianCalendarMember

  final override type Moment = GregorianMoment

  final override def createMoment(raw: RawNumber): Gregorian#Moment =
    new GregorianMoment(raw) with GregorianCalendarMember {
      final override def numberSystem: Gregorian = Gregorian.this
    }

  final override val Moment: GregorianMomentCompanion =
    new GregorianMomentCompanion with GregorianCalendarMember
}


object Gregorian extends Gregorian
