package org.opentorah.calendar

import org.opentorah.calendar.jewish.Jewish
import org.opentorah.metadata.{HasName, HasValues, Language, Named, Names}
import org.opentorah.numbers.Digits
import org.opentorah.util.Cache

trait Calendar extends Times, Epoch:
  // TODO load from a metadata resource
  def name: String

  trait Member:
    final def calendar: Calendar = Calendar.this

  // Note: YearBase, MonthBase and DayBase used to require their number to be positive;
  // because Roman years can be negative for years after Creation, YearBase can not;
  // because of the need to calculate Julian Date, neither can MonthBase and DayBase.

  type YearCharacter

  abstract class YearBase(final override val number: Int) extends
    Member,
    Numbered[Year],
    Language.ToString
    derives CanEqual:
    this: Year =>

    final override def companion: Numbered.Companion[Year] = Year

    def character: YearCharacter

    final def isLeap: Boolean = Year.isLeap(number)

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

    final override def toLanguageString(using spec: Language.Spec): String = intToString(number)

  type Year <: YearBase

  final val cacheYears: Boolean = true

  trait YearCompanion extends Numbered.Companion[Year]:
    final override def apply(number: Int): Year =
      yearsCache.get(number, cacheYears)

    private final val yearsCache: Cache[Int, Year] = (number: Int) => newYear(number)

    // Note: actual constructor - for when apply() gets a cache miss
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

  open class MonthBase(yearOption: Option[Year], final override val number: Int) extends
    Member,
    Numbered[Month]
    derives CanEqual:
    this: Month =>

    final override def companion: Numbered.Companion[Month] = Month

    private var yearOpt: Option[Year] = yearOption

    final def year: Year =
      if yearOpt.isEmpty then yearOpt = Some(Year(Month.yearNumber(number)))
      yearOpt.get

    final def numberInYear: Int = Month.numberInYear(number)

    final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

    final def firstDay: Day = day(1)

    final def lastDay: Day = day(length)

    final def days: Seq[Day] = (1 to length).map(day)

    final def day(numberInMonth: Int): Day = Day.witNumberInMonth(this, numberInMonth)

    final def name: Month.Name = descriptor.name

    final def length: Int = descriptor.length

    private def descriptor: MonthDescriptor = year.monthDescriptors(numberInYear - 1)

    final def numberInYearToLanguageString(using spec: Language.Spec): String = intToString(numberInYear)

  type Month <: MonthBase

  // TODO toString once all deriveds are enums - but making them such breaks Scala 3 compiler!!!
  open class MonthNameBase(nameOverride: Option[String]) extends
    Named.ByLoader[MonthName](loader = Month, nameOverride),
    HasName.NonEnum

  type MonthName <: MonthNameBase

  given CanEqual[MonthName, MonthName] = CanEqual.derived

  final class MonthNameAndLength(val name: Month.Name, val length: Int)

  final class MonthDescriptor(val name: Month.Name, val length: Int, val daysBefore: Int)

  final class MonthAndDay(val monthName: Month.Name, val numberInMonth: Int)

  abstract class MonthCompanion(resourceName: String) extends
    Names.Loader[MonthName](resourceNameOverride = Some(resourceName)),
    HasValues.FindByDefaultName[MonthName],
    HasValues.FindByName[MonthName],
    Numbered.Companion[Month]:

    final type Name = MonthName

    final override def apply(number: Int): Month = apply(None, number)

    private[opentorah] def apply(yearOpt: Option[Year], number: Int): Month

    private[Calendar] def yearNumber(monthNumber: Int): Int

    private[Calendar] final def withNumberInYear(year: Year, numberInYear: Int): Month =
      require(0 < numberInYear && numberInYear <= year.lengthInMonths)
      apply(Some(year), year.firstMonthNumber + numberInYear - 1)

    private[Calendar] def numberInYear(monthNumber: Int): Int

  type MonthCompanionType <: MonthCompanion

  final val Month: MonthCompanionType = createMonthCompanion

  protected def createMonthCompanion: MonthCompanionType

  open class DayBase(monthOption: Option[Month], final override val number: Int) extends
    Member,
    Numbered[Day],
    Language.ToString
    derives CanEqual:
    this: Day =>

    final override def companion: Numbered.Companion[Day] = Day

    private var monthOpt: Option[Month] = monthOption

    final def month: Month =
      if monthOpt.isEmpty then monthOpt = Some {
        // TODO remove magic constant 4
        var year: Year = Year((4 * number / (4 * Calendar.fullDaysInSolarYear + 1)) - 1)
        require(year.firstDayNumber <= number)

        while year.next.firstDayNumber <= number do year = year.next

        year.monthForDay(number - year.firstDayNumber + 1)
      }

      monthOpt.get

    @scala.annotation.targetName("subtract")
    final def -(that: Day): Int = this.number - that.number

    final def year: Year = month.year

    final def numberInYear: Int = number - year.firstDayNumber + 1

    final def numberInMonth: Int = number - month.firstDayNumber + 1

    final def numberInWeek: Int = Day.numberInWeek(number)

    final def to(calendar: Calendar): calendar.Day = if this.calendar eq calendar
      then this.asInstanceOf[calendar.Day]
      else calendar.Day(number + epochDifference(calendar))

    final def name: Week.Day = Week.Day.forNumber(numberInWeek)

    final def is(name: Week.Day): Boolean = this.name == name

    final def monthAndDay: MonthAndDay = MonthAndDay(month.name, numberInMonth)

    @scala.annotation.tailrec
    final def nextDay(dayName: Week.Day): Day = if is(dayName) then this else this.next.nextDay(dayName)

    @scala.annotation.tailrec
    final def prevDay(dayName: Week.Day): Day = if is(dayName) then this else this.prev.prevDay(dayName)

    // Note: Day numbering starts at 1; that is why 1 is subtracted here and added in MomentBase.dayNumber:
    final def toMoment: Moment = Moment().days(number - 1)

    final override def toLanguageString(using spec: Language.Spec): String =
      year.toLanguageString + " " +
      month.name.toLanguageString + " " +
      numberInMonthToLanguageString

    final def numberInMonthToLanguageString(using spec: Language.Spec): String = intToString(numberInMonth)

  type Day <: DayBase

  final class DayCompanion extends
    Member,
    Numbered.Companion[Day]:
    override def apply(number: Int): Day = newDay(None, number)

    private[Calendar] def witNumberInMonth(month: Month, numberInMonth: Int): Day =
      require (0 < numberInMonth && numberInMonth <= month.length)
      newDay(Some(month), month.firstDayNumber + numberInMonth - 1)

    // Note: change of day because of the time offset is not taken into account,
    // so careful with things like molad announcements...
    def numberInWeek(dayNumber: Int): Int =
      ((dayNumber - 1) + (Jewish.epochDayNumberInWeek - 1) + epochDifference(Jewish)) % Week.length + 1

  final val Day: DayCompanion = new DayCompanion

  protected def newDay(monthOpt: Option[Month], number: Int): Day

  abstract class MomentBase(digits: Digits) extends
    TimePointBase(digits),
    Member,
    Language.ToString:
    this: Moment =>

    final def day: Day = Day(dayNumber)

    // Note: Day numbering starts at 1; that is why 1 is added here and subtracted in DayBase.toMoment:
    final def dayNumber: Int = days + 1

    final def to(calendar: Calendar): calendar.Moment = if this.calendar eq calendar
      then this.asInstanceOf[calendar.Moment]
      else calendar.Moment.fromDigits(toEpoch(calendar))

    final def toJulianDay: Double = convertTo[Double](getDigits(toEpoch(Epoch.JulianDay)))

    private def toEpoch(epoch: Epoch): Digits = addDigits(Seq(
      epochDifference(epoch),
      epochHoursDifference(epoch)
    ))

    final override def toLanguageString(using spec: Language.Spec): String =
      day.toLanguageString +
        " " + intToString(time.hours) +
        ":" + intToString(time.minutes) +
        "." + intToString(time.partsWithoutMinutes) +
        "." + intToString(time.moments)

    final def toSecondLanguageString(using spec: Language.Spec): String =
      day.toLanguageString +
        " " + intToString(time.hours) +
        ":" + intToString(time.minutes) +
        ":" + intToString(time.seconds) +
        "." + intToString(time.milliseconds)

  type Moment <: MomentBase

  final override type Point = Moment

  final class MomentCompanion extends PointCompanion

  final override type PointCompanionType = MomentCompanion

  final override protected def createPointCompanion: PointCompanionType = new MomentCompanion

  final def Moment: MomentCompanion = Point

  final type TimeVector = Vector

  final def TimeVector: VectorCompanion = Vector

  def intToString(number: Int)(using spec: Language.Spec): String

object Calendar:
  final val fullDaysInSolarYear: Int = 365
