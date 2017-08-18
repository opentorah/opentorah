package org.podval.calendar.gregorian

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def calendar: Gregorian = Gregorian.this
  }

  abstract class GregorianYear(number: Int) extends YearBase[Gregorian](number) { this: Gregorian#Year =>
    final override def firstDayNumber: Int = Year.firstDay(number)

    final override def lengthInDays: Int = Year.lengthInDays(number)

    final override def character: Gregorian#YearCharacter = isLeap
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


  abstract class GregorianMonthCompanion extends MonthCompanion[Gregorian] {
    final override def yearNumber(monthNumber: Int): Int =
      (monthNumber - 1) / Gregorian.Year.monthsInYear + 1

    final override def numberInYear(monthNumber: Int): Int =
      monthNumber - Gregorian.Year.firstMonth(yearNumber(monthNumber)) + 1
  }


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


  abstract class GregorianMoment(raw: RawNumber)
    extends MomentBase[Gregorian](raw)
  { this: Gregorian#Moment =>
    final def morningHours(value: Int): Gregorian#Moment = firstHalfHours(value)

    final def afternoonHours(value: Int): Gregorian#Moment = secondHalfHours(value)
  }


  final override type Moment = GregorianMoment

  final override def createMoment(raw: RawNumber): Gregorian#Moment =
    new GregorianMoment(raw) with GregorianCalendarMember {
      final override def numberSystem: Gregorian = Gregorian.this
    }

  final override val Moment: GregorianMomentCompanion =
    new GregorianMomentCompanion with GregorianCalendarMember
}


object Gregorian extends Gregorian
