package org.opentorah.calendar.roman

import org.opentorah.calendar.Calendar
import org.opentorah.metadata.{LanguageSpec, Named, Names}

trait Roman extends Calendar {

  final override def epochHours: Int = 6

  final class RomanYear(number: Int) extends YearBase(number) {
    override def firstDayNumber: Int =
      daysInNonLeapYear * (number - 1) + yearFirstDayCorrection(number) + 1

    override def lengthInDays: Int =
      daysInNonLeapYear + (if (isLeapYear(number)) 1 else 0)

    override def character: YearCharacter = isLeap
  }

  final override type Year = RomanYear

  final override type YearCharacter = Boolean

  override protected def areYearsPositive: Boolean = false

  final class RomanYearCompanion extends YearCompanion {
    override protected def newYear(number: Int): Year =
      new RomanYear(number)

    override protected def characters: Seq[YearCharacter] =
      Seq(true, false)

    override protected def monthNamesAndLengths(isLeap: YearCharacter): Seq[MonthNameAndLength] = {
      Seq(
        new MonthNameAndLength(Month.January  , 31),
        new MonthNameAndLength(Month.February , if (isLeap) 29 else 28),
        new MonthNameAndLength(Month.March    , 31),
        new MonthNameAndLength(Month.April    , 30),
        new MonthNameAndLength(Month.May      , 31),
        new MonthNameAndLength(Month.June     , 30),
        new MonthNameAndLength(Month.July     , 31),
        new MonthNameAndLength(Month.August   , 31),
        new MonthNameAndLength(Month.September, 30),
        new MonthNameAndLength(Month.October  , 31),
        new MonthNameAndLength(Month.November , 30),
        new MonthNameAndLength(Month.December , 31)
      )
    }

    override def isLeap(yearNumber: Int): Boolean = isLeapYear(yearNumber)

    override def firstMonth(yearNumber: Int): Int =
      monthsInYear*(yearNumber - 1) + 1

    override def lengthInMonths(yearNumber: Int): Int = monthsInYear
  }

  final override lazy val Year: RomanYearCompanion = new RomanYearCompanion

  private val monthsInYear: Int = 12

  private val daysInNonLeapYear: Int = 365

  protected def yearFirstDayCorrection(yearNumber: Int): Int

  protected def isLeapYear(yearNumber: Int): Boolean

  trait RomanMonth extends MonthBase

  final override type Month = RomanMonth

  sealed trait RomanMonthName extends Named {
    final override def names: Names = Month.toNames(this)
  }

  final override type MonthName = RomanMonthName

  final class RomanMonthCompanion extends MonthCompanion {
    override private[opentorah] def apply(yearOption: Option[Year], monthNumber: Int): Month =
      new RomanMonth {
        override protected var yearOpt: Option[Year] = yearOption
        override def number: Int = monthNumber
      }

    override private[opentorah] def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / monthsInYear + 1

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

    override val values: Seq[Key] =
      Seq(January, February, March, April, May, June, July, August, September, October, November, December)

    protected override def resourceName: String = "RomanMonth"
  }

  final override lazy val Month: RomanMonthCompanion = new RomanMonthCompanion

  trait RomanDay extends DayBase

  final override type Day = RomanDay

  final class RomanDayCompanion extends DayCompanion {
    override private[opentorah] def apply(monthOption: Option[Month], dayNumber: Int): Day =
      new RomanDay {
        override protected var monthOpt: Option[Month] = monthOption
        override def number: Int = dayNumber
      }
  }

  final override lazy val Day: RomanDayCompanion = new RomanDayCompanion

  final class RomanMoment(digits: Seq[Int]) extends MomentBase(digits) {
    override def companion: RomanMomentCompanion = Point

    def morningHours(value: Int): Moment = firstHalfHours(value)

    def afternoonHours(value: Int): Moment = secondHalfHours(value)
  }

  final override type Moment = RomanMoment

  final class RomanMomentCompanion extends MomentCompanion {
    override protected def newNumber(digits: Seq[Int]): Point = new RomanMoment(digits)
  }

  final override lazy val Point: RomanMomentCompanion = new RomanMomentCompanion

  final override def Moment: RomanMomentCompanion = Point

  final override def inToString(number: Int)(implicit spec: LanguageSpec): String = number.toString
}
