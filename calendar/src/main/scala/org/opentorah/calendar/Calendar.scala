package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.metadata.{Language, LanguageSpec, Named, NamedCompanion, Numbered}
import org.opentorah.numbers.Digits
import org.opentorah.util.Cache

trait Calendar extends Times:

  // days before the start of the calendar
  def epoch: Int

  // hours offset; for example:
  //  Jewish:   6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0  1  2  3  4  5  6
  //  Roman :  |0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23| 0
  def epochHours: Int

  type YearCharacter

  abstract class YearBase(final override val number: Int) extends Numbered[Year], Language.ToString { this: Year =>

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

    final def containsMonth(name: Month.Name): Boolean =
      monthDescriptors.exists(_.name == name)

    final def month(name: Month.Name): Month =
      month(monthDescriptors.indexWhere(_.name == name) + 1)

    final def monthAndDay(when: MonthAndDay): Day =
      month(when.monthName).day(when.numberInMonth)

    private[Calendar] final def monthForDay(day: Int): Month =
      require(0 < day && day <= lengthInDays)
      month(monthDescriptors.count(_.daysBefore < day))

    private[Calendar] final def monthDescriptors: Seq[MonthDescriptor] =
      Year.monthDescriptors(character)

    final override def toLanguageString(using spec: LanguageSpec): String = intToString(number)
  }

  type Year <: YearBase

  protected def areYearsPositive: Boolean

  final val cacheYears: Boolean = true

  trait YearCompanion:
    private final val yearsCache: Cache[Int, Year] = new Cache[Int, Year]:
      override def calculate(number: Int): Year = newYear(number)

    final def apply(number: Int): Year =
      yearsCache.get(number, cacheYears)

    protected def newYear(number: Int): Year

    // lazy to make initialization work
    lazy val monthDescriptors: Map[YearCharacter, Seq[MonthDescriptor]] =
      Map(characters.map(character => character -> monthsGenerator(character))*)

    protected def characters: Seq[YearCharacter]

    private def monthsGenerator(character: YearCharacter): Seq[MonthDescriptor] =
      val namesAndLengths: Seq[MonthNameAndLength] = monthNamesAndLengths(character)
      val daysBeforeForMonth: Seq[Int] = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
      namesAndLengths zip daysBeforeForMonth map((nameAndLength, daysBefore) =>
        MonthDescriptor(nameAndLength.name, nameAndLength.length, daysBefore)
      )

    protected def monthNamesAndLengths(character: YearCharacter): Seq[MonthNameAndLength]

    protected final def yearLength(character: YearCharacter): Int =
      val lastMonth: MonthDescriptor = monthDescriptors(character).last
      lastMonth.daysBefore + lastMonth.length

    def isLeap(yearNumber: Int): Boolean

    def firstMonth(yearNumber: Int): Int

    def lengthInMonths(yearNumber: Int): Int

  type YearCompanionType <: YearCompanion

  final val Year: YearCompanionType = createYearCompanion

  protected def createYearCompanion: YearCompanionType

  open class MonthBase(yearOption: Option[Year], final override val number: Int) extends Numbered[Month] { this: Month =>

    protected var yearOpt: Option[Year] = yearOption

    require(0 < number)

    final def year: Year =
      if yearOpt.isEmpty then yearOpt = Some(Year(Month.yearNumber(number)))
      yearOpt.get

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

    final def name: Month.Name = descriptor.name

    final def length: Int = descriptor.length

    private def descriptor: MonthDescriptor = year.monthDescriptors(numberInYear - 1)

    final def numberInYearToLanguageString(using spec: LanguageSpec): String = intToString(numberInYear)
  }

  type Month <: MonthBase

  type MonthName <: Named

  final class MonthNameAndLength(val name: Month.Name, val length: Int)

  final class MonthDescriptor(val name: Month.Name, val length: Int, val daysBefore: Int)

  final class MonthAndDay(val monthName: Month.Name, val numberInMonth: Int)

  trait MonthCompanion extends NamedCompanion:
    final override type Key = MonthName
    final type Name = MonthName

    final def apply(number: Int): Month = apply(None, number)
    private[opentorah] def apply(yearOpt: Option[Year], number: Int): Month

    private[Calendar] def yearNumber(monthNumber: Int): Int

    private[Calendar] final def withNumberInYear(year: Year, numberInYear: Int): Month =
      require(0 < numberInYear && numberInYear <= year.lengthInMonths)
      apply(Some(year), year.firstMonthNumber + numberInYear - 1)

    private[Calendar] def numberInYear(monthNumber: Int): Int

  type MonthCompanionType <: MonthCompanion

  final val Month: MonthCompanionType = createMonthCompanion

  protected def createMonthCompanion: MonthCompanionType

  open class DayBase(monthOption: Option[Month], final override val number: Int) extends Numbered[Day], Language.ToString { this: Day =>

    protected var monthOpt: Option[Month] = monthOption

    require(0 < number)

    final def calendar: Calendar = Calendar.this

    final def month: Month =
      if monthOpt.isEmpty then monthOpt = Some {
        // TODO remove magic constant
        var year: Year = Year(scala.math.max(if areYearsPositive then 1 else 0, (4 * number / (4 * 365 + 1)) - 1))
        require(year.firstDayNumber <= number)

        while year.next.firstDayNumber <= number do year = year.next

        year.monthForDay(number - year.firstDayNumber + 1)
      }

      monthOpt.get

    final def next: Day = this + 1

    final def prev: Day = this - 1

    final def +(change: Int): Day = Day(number + change)

    final def -(change: Int): Day = Day(number - change)

    final def -(that: Day): Int = this.number - that.number

    final def year: Year = month.year

    final def numberInYear: Int = number - year.firstDayNumber + 1

    final def numberInMonth: Int = number - month.firstDayNumber + 1

    final def numberInWeek: Int = Day.numberInWeek(number)

    final def to(calendar: Calendar): calendar.Day =
      if this.calendar == calendar then this.asInstanceOf[calendar.Day]
      else calendar.Day(number + epoch - calendar.epoch)

    final def name: Week.Day = Week.Day.forNumber(numberInWeek)

    final def is(name: Week.Day): Boolean = this.name == name

    final def monthAndDay: MonthAndDay = MonthAndDay(month.name, numberInMonth)

    @scala.annotation.tailrec
    final def next(dayName: Week.Day): Day = if is(dayName) then this else this.next.next(dayName)

    @scala.annotation.tailrec
    final def prev(dayName: Week.Day): Day = if is(dayName) then this else this.prev.prev(dayName)

    // Note: Day numbering starts at 1; that is why 1 is subtracted here and added MomentBase.dayNumber:
    final def toMoment: Moment = Moment().days(number - 1)

    final override def toLanguageString(using spec: LanguageSpec): String =
      year.toLanguageString + " " + month.name.toLanguageString + " " + numberInMonthToLanguageString

    final def numberInMonthToLanguageString(using spec: LanguageSpec): String = intToString(numberInMonth)
  }

  type Day <: DayBase

  final class DayCompanion:
    def apply(number: Int): Day = newDay(None, number)

    private[Calendar] def witNumberInMonth(month: Month, numberInMonth: Int): Day =
      require (0 < numberInMonth && numberInMonth <= month.length)
      newDay(Some(month), month.firstDayNumber + numberInMonth - 1)

    // Note: change of day because of the time offset is not taken into account,
    // so careful with things like molad announcements...
    def numberInWeek(dayNumber: Int): Int =
      ((dayNumber - 1) + (Jewish.epochDayNumberInWeek - 1) + epoch - Jewish.epoch) % Week.length + 1

  final val Day: DayCompanion = new DayCompanion

  protected def newDay(monthOpt: Option[Month], number: Int): Day

  abstract class MomentBase(digits: Digits) extends TimePointBase(digits), Language.ToString { this: Moment =>
    final def calendar: Calendar = Calendar.this

    final def day: Day = Day(dayNumber)

    // Note: Day numbering starts at 1; that is why 1 is added here and subtracted in DayBase.toMoment:
    final def dayNumber: Int = days + 1

    def to(calendar: Calendar): calendar.Moment = if this.calendar == calendar then this.asInstanceOf[calendar.Moment] else
      // TODO this looks like Digits addition with carry, and should be implemented that way...
      val toHours: Int = hours + epochHours - calendar.epochHours

      val (newDay, newHours) =
        if hours < 0                 then (day.prev, toHours + Times.hoursPerDay) else
        if hours > Times.hoursPerDay then (day.next, toHours - Times.hoursPerDay) else
                                          (day     , toHours                    )

      newDay.to(calendar).toMoment.hours(newHours).parts(parts)

    final override def toLanguageString(using spec: LanguageSpec): String =
      day.toLanguageString +
        " " + intToString(time.hours) +
        ":" + intToString(time.minutes) +
        "." + intToString(time.partsWithoutMinutes) +
        "." + intToString(time.moments)

    final def toSecondLanguageString(using spec: LanguageSpec): String =
      day.toLanguageString +
        " " + intToString(time.hours) +
        ":" + intToString(time.minutes) +
        ":" + intToString(time.seconds) +
        "." + intToString(time.milliseconds)
  }

  type Moment <: MomentBase

  final override type Point = Moment

  final class MomentCompanion extends PointCompanion

  final override type PointCompanionType = MomentCompanion

  final override protected def createPointCompanion: PointCompanionType = new MomentCompanion

  final def Moment: MomentCompanion = Point
  
  final type TimeVector = Vector
  
  final def TimeVector: VectorCompanion = Vector

  def intToString(number: Int)(using spec: LanguageSpec): String

