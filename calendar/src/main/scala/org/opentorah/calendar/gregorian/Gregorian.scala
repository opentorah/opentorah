package org.opentorah.calendar.gregorian

import org.opentorah.metadata.{LanguageSpec, Named, NamedCompanion, Names}
import org.opentorah.dates.Calendar
import org.opentorah.numbers.BigRational

object Gregorian extends Calendar {

  final class GregorianYear(number: Int) extends YearBase(number) {
    override def firstDayNumber: Int = Year.firstDay(number)

    override def lengthInDays: Int = Year.lengthInDays(number)

    override def character: YearCharacter = isLeap
  }

  final override type Year = GregorianYear

  final override type YearCharacter = Boolean

  final class GregorianYearCompanion extends YearCompanion {
    override protected def newYear(number: Int): Year =
      new GregorianYear(number)

    override protected def characters: Seq[YearCharacter] =
      Seq(true, false)

    override protected def monthNamesAndLengths(isLeap: YearCharacter): Seq[MonthNameAndLength] = {
      Seq(
        new MonthNameAndLength(Month.Name.January  , 31),
        new MonthNameAndLength(Month.Name.February , if (isLeap) 29 else 28),
        new MonthNameAndLength(Month.Name.March    , 31),
        new MonthNameAndLength(Month.Name.April    , 30),
        new MonthNameAndLength(Month.Name.May      , 31),
        new MonthNameAndLength(Month.Name.June     , 30),
        new MonthNameAndLength(Month.Name.July     , 31),
        new MonthNameAndLength(Month.Name.August   , 31),
        new MonthNameAndLength(Month.Name.September, 30),
        new MonthNameAndLength(Month.Name.October  , 31),
        new MonthNameAndLength(Month.Name.November , 30),
        new MonthNameAndLength(Month.Name.December , 31)
      )
    }

    override protected def areYearsPositive: Boolean = false

    override def isLeap(yearNumber: Int): Boolean =
      (yearNumber % 4 == 0) && ((yearNumber % 100 != 0) || (yearNumber % 400 == 0))

    override def firstMonth(yearNumber: Int): Int =
      monthsInYear*(yearNumber - 1) + 1

    override def lengthInMonths(yearNumber: Int): Int = monthsInYear

    val monthsInYear: Int = 12

    private val daysInNonLeapYear: Int = 365

    def firstDay(yearNumber: Int): Int =
      daysInNonLeapYear * (yearNumber - 1) + (yearNumber - 1)/4 - (yearNumber - 1)/100 +
        (yearNumber - 1)/400 + 1

    def lengthInDays(yearNumber: Int): Int =
      if (Year.isLeap(yearNumber)) daysInNonLeapYear + 1 else daysInNonLeapYear

    lazy val yearLength: TimeVector = TimeVector.fromRational(
      BigRational(365) +
      BigRational(1, 4) -
      BigRational(1, 100) +
      BigRational(1, 400),
      length = maxLength
    )
  }

  final override lazy val Year = new GregorianYearCompanion

  trait GregorianMonth extends MonthBase

  final override type Month = GregorianMonth

  final class GregorianMonthCompanion extends MonthCompanion {
    override private[opentorah] def apply(yearOption: Option[Year], monthNumber: Int): Month =
      new GregorianMonth {
        override protected var yearOpt: Option[Year] = yearOption
        override def number: Int = monthNumber
      }

    override val Name: GregorianMonthCompanion.type = GregorianMonthCompanion

    override private[opentorah] def yearNumber(monthNumber: Int): Int = (monthNumber - 1) / Year.monthsInYear + 1

    override private[opentorah] def numberInYear(monthNumber: Int): Int =
      monthNumber - Year.firstMonth(yearNumber(monthNumber)) + 1
  }

  object GregorianMonthCompanion extends NamedCompanion {
    sealed trait Key extends Named {
      final override def names: Names = toNames(this)
    }

    case object January extends Key
    case object February extends Key
    case object March extends Key
    case object April extends Key
    case object May extends Key
    case object June extends Key
    case object July extends Key
    case object August extends Key
    case object September extends Key
    case object October extends Key
    case object November extends Key
    case object December extends Key

    override val values: Seq[Key] =
      Seq(January, February, March, April, May, June, July, August, September, October, November, December)

    protected override def resourceName: String = "GregorianMonth"
  }

  final override lazy val Month = new GregorianMonthCompanion

  trait GregorianDay extends DayBase

  final override type Day = GregorianDay

  final class GregorianDayCompanion extends DayCompanion {
    override private[opentorah] def apply(monthOption: Option[Month], dayNumber: Int): Day =
      new GregorianDay {
        override protected var monthOpt: Option[Month] = monthOption
        override def number: Int = dayNumber
      }

    override val Name: GregorianDayCompanion.type = GregorianDayCompanion

    override def names: Seq[Name] = GregorianDayCompanion.values

    override val firstDayNumberInWeek: Int = Calendar.firstDayNumberInWeekGregorian
  }

  object GregorianDayCompanion extends NamedCompanion {
    sealed trait Key extends Named {
      final override def names: Names = toNames(this)
    }

    case object Sunday extends Key
    case object Monday extends Key
    case object Tuesday extends Key
    case object Wednesday extends Key
    case object Thursday extends Key
    case object Friday extends Key
    case object Saturday extends Key

    override val values: Seq[Key] = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

    protected override def resourceName: String = "GregorianDay"
  }

  final override lazy val Day = new GregorianDayCompanion

  final class GregorianMoment(digits: Seq[Int]) extends MomentBase(digits) {
    override def companion: GregorianMomentCompanion = Point

    def morningHours(value: Int): Moment = firstHalfHours(value)

    def afternoonHours(value: Int): Moment = secondHalfHours(value)
  }

  final override type Point = GregorianMoment

  final class GregorianMomentCompanion extends MomentCompanion {
    override protected def newNumber(digits: Seq[Int]): Point = new GregorianMoment(digits)
  }

  final override lazy val Point: GregorianMomentCompanion = new GregorianMomentCompanion

  final override def toString(number: Int)(implicit spec: LanguageSpec): String = number.toString
}
