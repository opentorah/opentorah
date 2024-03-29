package org.opentorah.calendar.roman

import org.opentorah.calendar.Calendar
import org.opentorah.metadata.{Language, Named, Names}

trait Roman extends Calendar:

  final override def epochHours: Int = 6

  final class RomanYear(number: Int) extends YearBase(number):

    override def firstDayNumber: Int =
      val correctionForLeapYears: Int = if number > 0
        then   numberOfLeapYears( number-1)
        else - numberOfLeapYears(-number  ) - 1 // year 0 is leap

      Calendar.fullDaysInSolarYear * (number - 1) + correctionForLeapYears + 1

    override def lengthInDays: Int =
      Calendar.fullDaysInSolarYear + (if isLeap then 1 else 0)

    override def character: YearCharacter = isLeap

  final override type Year = RomanYear

  final override type YearCharacter = Boolean

  final class RomanYearCompanion extends YearCompanion:
    override protected def newYear(number: Int): Year = RomanYear(number)

    override protected def characters: Seq[YearCharacter] =
      Seq(true, false)

    override protected def monthNamesAndLengths(isLeap: YearCharacter): Seq[MonthNameAndLength] =
      Seq(
        MonthNameAndLength(Month.January  , 31),
        MonthNameAndLength(Month.February , if isLeap then 29 else 28),
        MonthNameAndLength(Month.March    , 31),
        MonthNameAndLength(Month.April    , 30),
        MonthNameAndLength(Month.May      , 31),
        MonthNameAndLength(Month.June     , 30),
        MonthNameAndLength(Month.July     , 31),
        MonthNameAndLength(Month.August   , 31),
        MonthNameAndLength(Month.September, 30),
        MonthNameAndLength(Month.October  , 31),
        MonthNameAndLength(Month.November , 30),
        MonthNameAndLength(Month.December , 31)
      )

    override def isLeap(yearNumber: Int): Boolean = isLeapYear(yearNumber)

    override def firstMonth(yearNumber: Int): Int =
      monthsInYear*(yearNumber - 1) + 1

    override def lengthInMonths(yearNumber: Int): Int = monthsInYear

  final override type YearCompanionType = RomanYearCompanion

  final override protected def createYearCompanion: YearCompanionType = new RomanYearCompanion

  private val monthsInYear: Int = 12

  protected def numberOfLeapYears(yearNumber: Int /* non-negative */): Int

  protected def isLeapYear(yearNumber: Int): Boolean

  final class RomanMonth(yearOptInitial: Option[Year], monthNumber: Int) extends MonthBase(yearOptInitial, monthNumber)

  final override type Month = RomanMonth

  sealed class RomanMonthName(nameOverride: Option[String] = None) extends MonthNameBase(nameOverride)

  final override type MonthName = RomanMonthName

  object RomanMonthName extends MonthCompanion(resourceName = "RomanMonth"):
    override private[opentorah] def apply(yearOption: Option[Year], monthNumber: Int): Month =
      RomanMonth(yearOption, monthNumber)

    override private[opentorah] def yearNumber(monthNumber: Int): Int = if monthNumber > 0
      then (monthNumber - 1) / monthsInYear + 1
      else  monthNumber      / monthsInYear

    override private[opentorah] def numberInYear(monthNumber: Int): Int =
      monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1

    case object January   extends RomanMonthName
    case object February  extends RomanMonthName
    case object March     extends RomanMonthName
    case object April     extends RomanMonthName
    case object May       extends RomanMonthName
    case object June      extends RomanMonthName
    case object July      extends RomanMonthName
    case object August    extends RomanMonthName
    case object September extends RomanMonthName
    case object October   extends RomanMonthName
    case object November  extends RomanMonthName
    case object December  extends RomanMonthName

    override val valuesSeq: Seq[Name] =
      Seq(January, February, March, April, May, June, July, August, September, October, November, December)

  final override type MonthCompanionType = RomanMonthName.type

  final override protected def createMonthCompanion: MonthCompanionType = RomanMonthName

  final class RomanDay(monthOption: Option[Month], dayNumber: Int) extends DayBase(monthOption, dayNumber)

  final override type Day = RomanDay

  final override protected def newDay(monthOption: Option[Month], dayNumber: Int): Day = RomanDay(monthOption, dayNumber)

  final class RomanMoment(digits: Seq[Int]) extends MomentBase(digits):
    def morningHours(value: Int): Moment = firstHalfHours(value)

    def afternoonHours(value: Int): Moment = secondHalfHours(value)

  final override type Moment = RomanMoment

  final override protected def newPoint(digits: Seq[Int]): Point = RomanMoment(digits)

  final override def intToString(number: Int)(using spec: Language.Spec): String = number.toString
