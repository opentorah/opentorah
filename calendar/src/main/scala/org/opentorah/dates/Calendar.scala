package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString, NamedCompanion, Numbered}
import org.opentorah.numbers.Digits
import org.opentorah.times.Times
import org.opentorah.util.Cache

trait Calendar extends Times {

  /**
   *
   * @param yearNumber  number of the Year
   */
  abstract class YearBase(yearNumber: Int) extends Numbered with LanguageString { this: Year =>

    type T = Year

    override def number: Int = yearNumber

    def character: YearCharacter

    final def isLeap: Boolean = Year.isLeap(number)

    final def next: Year = this + 1

    final def prev: Year = this - 1

    final def +(change: Int): Year = Year(number + change)

    final def -(change: Int): Year = Year(number - change)

    final def firstDay: Day = firstMonth.firstDay

    final def lastDay: Day = lastMonth.lastDay

    def firstDayNumber: Int

    def lengthInDays: Int

    final def days: Seq[Day] = months.flatMap(_.days)

    final def firstMonth: Month = month(1)

    final def lastMonth: Month = month(lengthInMonths)

    final def firstMonthNumber: Int = Year.firstMonth(number)

    final def lengthInMonths: Int = Year.lengthInMonths(number)

    final def months: Seq[Month] = (1 to lengthInMonths).map(month)

    final def month(numberInYear: Int): Month =
      Month.withNumberInYear(this, numberInYear)

    final def containsMonth(name: MonthName): Boolean =
      monthDescriptors.exists(_.name == name)

    final def month(name: MonthName): Month =
      month(monthDescriptors.indexWhere(_.name == name) + 1)

    final def monthAndDay(when: MonthAndDay): Day =
      month(when.monthName).day(when.numberInMonth)

    private[opentorah] final def monthForDay(day: Int): Month = {
      require(0 < day && day <= lengthInDays)
      month(monthDescriptors.count(_.daysBefore < day))
    }

    private[opentorah] final def monthDescriptors: Seq[MonthDescriptor] =
      Year.monthDescriptors(character)

    final override def toLanguageString(implicit spec: LanguageSpec): String = Calendar.this.toString(number)
  }

  type Year <: YearBase

  type YearCharacter

  final val cacheYears: Boolean = true

  trait YearCompanion {
    private final val yearsCache: Cache[Int, Year] = new Cache[Int, Year] {
      override def calculate(number: Int): Year = newYear(number)
    }

    final def apply(number: Int): Year =
      yearsCache.get(number, cacheYears)

    protected def newYear(number: Int): Year

    // lazy to make initialization work
    lazy val monthDescriptors: Map[YearCharacter, Seq[MonthDescriptor]] =
      Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)

    protected def characters: Seq[YearCharacter]

    private[this] def monthsGenerator(character: YearCharacter): Seq[MonthDescriptor] = {
      val namesAndLengths = monthNamesAndLengths(character)
      val daysBeforeForMonth: Seq[Int] = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
      namesAndLengths zip daysBeforeForMonth map { case (nameAndLength, daysBefore) =>
        // TODO get rid of the cast!
        new MonthDescriptor(nameAndLength.name.asInstanceOf[Month.Name], nameAndLength.length, daysBefore)
      }
    }

    protected def monthNamesAndLengths(character: YearCharacter): Seq[MonthNameAndLength]

    protected final def yearLength(character: YearCharacter): Int = {
      val lastMonth: MonthDescriptor = monthDescriptors(character).last
      lastMonth.daysBefore + lastMonth.length
    }

    protected def areYearsPositive: Boolean

    final def yearsForSureBefore(dayNumber: Int): Int =  {
      val result: Int = (4 * dayNumber / (4 * 365 + 1)) - 1
      if (areYearsPositive) scala.math.max(1, result) else result
    }

    def isLeap(yearNumber: Int): Boolean

    def firstMonth(yearNumber: Int): Int

    def lengthInMonths(yearNumber: Int): Int
  }

  val Year: YearCompanion

  trait MonthBase extends Numbered { this: Month =>

    type T = Month

    protected var yearOpt: Option[Year]

    require(0 < number)

    final def year: Year = {
      if (yearOpt.isEmpty) {
        yearOpt = Some(Year(Month.yearNumber(number)))
      }

      yearOpt.get
    }

    final def next: Month = this + 1

    final def prev: Month = this - 1

    final def +(change: Int): Month = Month(number + change)

    final def -(change: Int): Month = Month(number - change)

    final def numberInYear: Int = Month.numberInYear(number)

    final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

    final def firstDay: Day = day(1)

    final def lastDay: Day = day(length)

    final def days: Seq[Day] = (1 to length).map(day)

    final def day(numberInMonth: Int): Day = Day.witNumberInMonth(this, numberInMonth)

    final def name: MonthName = descriptor.name

    final def length: Int = descriptor.length

    private[this] def descriptor: MonthDescriptor = year.monthDescriptors(numberInYear - 1)

    final def numberInYearToLanguageString(implicit spec: LanguageSpec): String = Calendar.this.toString(numberInYear)
  }

  type Month <: MonthBase

  final class MonthNameAndLength(val name: Month.Name, val length: Int)

  final class MonthDescriptor(val name: Month.Name, val length: Int, val daysBefore: Int)

  final class MonthAndDay(val monthName: Month.Name, val numberInMonth: Int)

  // TODO remove?
  final type MonthName = Month.Name

  trait MonthCompanion {
    val Name: NamedCompanion

    final type Name = Name.Key

    final def apply(number: Int): Month = apply(None, number)

    private[opentorah] def yearNumber(monthNumber: Int): Int

    private[opentorah] final def withNumberInYear(year: Year, numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= year.lengthInMonths)
      apply(Some(year), year.firstMonthNumber + numberInYear - 1)
    }

    private[opentorah] def apply(yearOpt: Option[Year], number: Int): Month

    private[opentorah] def numberInYear(monthNumber: Int): Int
  }

  val Month: MonthCompanion

  trait DayBase extends Numbered with LanguageString { this: Day =>

    type T = Day

    protected var monthOpt: Option[Month]

    require(0 < number)

    final def month: Month = {
      if (monthOpt.isEmpty) {
        var year: Year = Year(Year.yearsForSureBefore(number))
        require(year.firstDayNumber <= number)
        while (year.next.firstDayNumber <= number) year = year.next

        monthOpt = Some(year.monthForDay(number - year.firstDayNumber + 1))
      }

      monthOpt.get
    }

    final def next: Day = this + 1

    final def prev: Day = this - 1

    final def +(change: Int): Day = Day(number + change)

    final def -(change: Int): Day = Day(number - change)

    final def -(that: Day): Int = this.number - that.number

    final def year: Year = month.year

    final def numberInYear: Int = number - year.firstDayNumber + 1

    final def numberInMonth: Int = number - month.firstDayNumber + 1

    final def numberInWeek: Int = Day.numberInWeek(number)

    final def name: DayName = Day.names(numberInWeek - 1)

    final def is(name: DayName): Boolean = this.name == name

    final def monthAndDay: MonthAndDay = new MonthAndDay(month.name, numberInMonth)

    @scala.annotation.tailrec
    final def next(dayName: DayName): Day = if (is(dayName)) this else this.next.next(dayName)

    @scala.annotation.tailrec
    final def prev(dayName: DayName): Day = if (is(dayName)) this else this.prev.prev(dayName)

    final def toMoment: Moment = Moment().days(number - 1)

    final override def toLanguageString(implicit spec: LanguageSpec): String =
      year.toLanguageString + " " + month.name.toLanguageString + " " + numberInMonthToLanguageString

    final def numberInMonthToLanguageString(implicit spec: LanguageSpec): String = Calendar.this.toString(numberInMonth)
  }

  type Day <: DayBase

  // TODO remove?
  final type DayName = Day.Name

  trait DayCompanion {
    val Name: NamedCompanion

    final type Name = Name.Key

    def names: Seq[DayName]

    final def apply(number: Int): Day = apply(None, number)

    private[opentorah] final def witNumberInMonth(month: Month, numberInMonth: Int): Day = {
      require (0 < numberInMonth && numberInMonth <= month.length)
      apply(Some(month), month.firstDayNumber + numberInMonth - 1)
    }

    private[opentorah] def apply(monthOpt: Option[Month], number: Int): Day

    final def numberInWeek(dayNumber: Int): Int =
      ((dayNumber + firstDayNumberInWeek - 1 - 1) % Calendar.daysPerWeek) + 1

    val firstDayNumberInWeek: Int
  }

  val Day: DayCompanion

  final type Moment = Point

  abstract class MomentBase(digits: Digits) extends TimePointBase(digits) with LanguageString { this: Moment =>

    final def day: Day = Day(dayNumber)

    final def dayNumber: Int = days + 1

    final override def toLanguageString(implicit spec: LanguageSpec): String =
      day.toLanguageString +
        " " + toString(time.hours) +
        ":" + toString(time.minutes) +
        "." + toString(time.partsWithoutMinutes) +
        "." + toString(time.moments)

    final def toSecondLanguageString(implicit spec: LanguageSpec): String =
      day.toLanguageString +
        " " + toString(time.hours) +
        ":" + toString(time.minutes) +
        ":" + toString(time.seconds) +
        "." + toString(time.milliseconds)
  }

  override type Point <: MomentBase

  trait MomentCompanion extends PointCompanion

  override val Point: MomentCompanion

  final lazy val Moment: MomentCompanion = Point

  final override type Vector = TimeVectorBase

  final type TimeVector = Vector

  final override lazy val Vector: VectorCompanion = new VectorCompanion {
    protected override def newNumber(digits: Digits): Vector =
      new TimeVectorBase(digits) {
        final override def companion: VectorCompanion = TimeVector
      }
  }

  final val TimeVector: VectorCompanion = Vector

  def toString(number: Int)(implicit spec: LanguageSpec): String
}


object Calendar {
  final val daysPerWeek: Int = 7

  // It seems that first day of the first year was Sunday; molad - BaHaRad.
  // Second year - Friday; molad - 8 in the morning.
  final val firstDayNumberInWeekJewish: Int = 1

  final val epoch: Int = 1373429

  final val firstDayNumberInWeekGregorian: Int =
    (((firstDayNumberInWeekJewish - 1) + epoch % daysPerWeek) % daysPerWeek) + 1
}
