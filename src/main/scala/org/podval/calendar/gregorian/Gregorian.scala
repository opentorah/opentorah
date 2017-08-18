package org.podval.calendar.gregorian

import org.podval.calendar.calendar._
import org.podval.calendar.numbers.NumberSystem.RawNumber

class Gregorian private() extends Calendar[Gregorian] {

  trait GregorianCalendarMember extends CalendarMember[Gregorian] {
    final override def calendar: Gregorian = Gregorian.this
  }

  abstract class GregorianYear(number: Int) extends YearBase(number) { this: Gregorian#Year =>
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

  abstract class GregorianYearCompanion extends YearCompanion[Gregorian] {
    protected final override def characters: Seq[Gregorian#YearCharacter] =
      Seq(true, false)

    protected final override def monthNamesAndLengths(isLeap: Gregorian#YearCharacter):
      List[MonthNameAndLength] =
    {
      import MonthName._
      List(
        MonthNameAndLength(January  , 31),
        MonthNameAndLength(February , if (isLeap) 29 else 28),
        MonthNameAndLength(March    , 31),
        MonthNameAndLength(April    , 30),
        MonthNameAndLength(May      , 31),
        MonthNameAndLength(June     , 30),
        MonthNameAndLength(July     , 31),
        MonthNameAndLength(August   , 31),
        MonthNameAndLength(September, 30),
        MonthNameAndLength(October  , 31),
        MonthNameAndLength(November , 30),
        MonthNameAndLength(December , 31)
      )
    }

    protected final override def areYearsPositive: Boolean = false

    final override def isLeap(yearNumber: Int): Boolean =
      (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

    final override def firstMonth(yearNumber: Int): Int =
      monthsInYear*(yearNumber - 1) + 1

    final override def lengthInMonths(yearNumber: Int): Int = monthsInYear

    val monthsInYear: Int = 12

    private val daysInNonLeapYear: Int = 365

    final def firstDay(yearNumber: Int): Int =
      daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 +
        (yearNumber - 1)/400 + 1

    final def lengthInDays(yearNumber: Int): Int =
      if (Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear
  }


  final override type Month = GregorianMonth

  final override def createMonth(number: Int): Gregorian#Month =
    new GregorianMonth(number) with GregorianCalendarMember

  final override type MonthName = GregorianMonthName

  // TODO stick it into the Month companion???
  val MonthName: GregorianMonthName.type = GregorianMonthName

  abstract class GregorianMonthCompanion extends MonthCompanion {
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
    extends MomentBase(raw) with GregorianCalendarMember
      // TODO prefix with C# when feasible...
  { this: Moment =>
    final def morningHours(value: Int): Moment = firstHalfHours(value)

    final def afternoonHours(value: Int): Moment = secondHalfHours(value)
  }

  final override type Moment = GregorianMoment

  final override def createMoment(raw: RawNumber): Moment =
    new GregorianMoment(raw) with GregorianCalendarMember

  final override val Moment: GregorianMomentCompanion =
    new GregorianMomentCompanion with GregorianCalendarMember
}


object Gregorian extends Gregorian
