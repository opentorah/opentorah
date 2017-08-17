package org.podval.calendar.dates

trait Calendar[C <: Calendar[C]] { this: C =>

  type Year <: YearBase

  type Month <: MonthBase

  type Day <: DayBase[C]

  type Moment <: MomentBase


  /**
   *
   * @param number  of the Year
   */
  abstract class YearBase(number: Int)
    extends Numbered[Year](number) with CalendarMember[C]
  { this: Year =>
    def character: YearCharacter

    final def isLeap: Boolean = Year.isLeap(number)

    final def next: Year = Year(number + 1)

    final def prev: Year = Year(number - 1)

    final def +(change: Int) = Year(number + change)

    final def -(change: Int) = Year(number - change)

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

    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      Month(firstMonthNumber + numberInYear - 1)
    }

    // TODO if YearBase is split into separate file (and parameterized with [C <: Calendar[C]]),
    // type of the month() method parameter becomes `calendar.Month.Name`, causing compilation error
    // (stable identifier required).
    // Conclusion: Month.Name type has to be moved into the Calendar itself first :(
    final def month(name: MonthName): Month = month(monthDescriptors.indexWhere(_.name == name) + 1)

    final def monthForDay(day: Int): Month = {
      require(0 < day && day <= lengthInDays)
      month(monthDescriptors.count(_.daysBefore < day))
    }

    final def monthDescriptors: List[MonthDescriptor] = Year.monthDescriptors(character)
  }


  type YearCharacter

  /**
   *
   */
  abstract class YearCompanionBase {
    def apply(number: Int): Year

    final  def apply(month: Month): Year = apply(Month.yearNumber(month.number))

    final def apply(day: C#Day): Year = {
      var result = apply(yearsForSureBefore(day.number))
      require(result.firstDayNumber <= day.number)
      while (result.next.firstDayNumber <= day.number) result = result.next
      result
    }

    val monthDescriptors: Map[YearCharacter, List[MonthDescriptor]] =
      Map((for (character <- characters) yield character -> monthsGenerator(character)): _*)

    protected def characters: Seq[YearCharacter]

    private[this] def monthsGenerator(character: YearCharacter): List[MonthDescriptor] = {
      val namesAndLengths = monthNamesAndLengths(character)
      // TODO dayses?
      val daysesBefore = namesAndLengths.map(_.length).scanLeft(0)(_ + _).init
      namesAndLengths zip daysesBefore map { case (nameAndLength, daysBefore) =>
        new MonthDescriptor(nameAndLength.name, nameAndLength.length, daysBefore)
      }
    }

    protected def monthNamesAndLengths(character: YearCharacter): List[MonthNameAndLength]

    protected def areYearsPositive: Boolean

    private[this] final def yearsForSureBefore(dayNumber: Int): Int =  {
      val result = (4 * dayNumber / (4 * 365 + 1)) - 1
      if (areYearsPositive) scala.math.max(1, result) else result
    }

    def isLeap(yearNumber: Int): Boolean

    def firstMonth(yearNumber: Int): Int

    def lengthInMonths(yearNumber: Int): Int
  }

  val Year: YearCompanionBase


  /**
   *
   * @param number  of the Month
   */
  abstract class MonthBase(number: Int)
    extends Numbered[Month](number) with CalendarMember[C]
  { this: Month =>
    require(0 < number)

    final def next: Month = Month(number + 1)

    final def prev: Month = Month(number - 1)

    final def +(change: Int) = Month(number + change)

    final def -(change: Int) = Month(number - change)

    final def year: Year = Year(this)

    final def numberInYear: Int = Month.numberInYear(number)

    final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

    final def firstDay: Day = day(1)

    final def lastDay: Day = day(length)

    final def days: Seq[Day] = (1 to length).map(day)

    final def day(numberInMonth: Int): Day = {
      require (0 < numberInMonth && numberInMonth <= length)
      Day(firstDayNumber + numberInMonth - 1)
    }

    final def name: MonthName = descriptor.name

    final def length: Int = descriptor.length

    private[this] def descriptor = year.monthDescriptors(numberInYear - 1)
  }


  type MonthName


  /**
   *
   */
  abstract class MonthCompanion {
    def apply(number: Int): Month

    final def apply(year: Int, monthInYear: Int): Month = Year(year).month(monthInYear)

    def yearNumber(monthNumber: Int): Int

    def numberInYear(monthNumber: Int): Int
  }


  final case class MonthNameAndLength(name: MonthName, length: Int)


  final      class MonthDescriptor   (val name: MonthName, val length: Int, val daysBefore: Int)


  val Month: MonthCompanion


  // TODO make this a Enum - and use its `values()` method in DayCompanion.name
  //   (which will then become `final`)?
  type DayName


  /**
   *
   */
  abstract class DayCompanion {
    val daysPerWeek: Int = 7

    def names: Seq[DayName]

    def apply(number: Int): Day

    final def apply(year: Int, month: MonthName, day: Int): Day = Year(year).month(month).day(day)

    final def apply(year: Int, month: Int, day: Int): Day = Year(year).month(month).day(day)

    final def numberInWeek(dayNumber: Int): Int = ((dayNumber + firstDayNumberInWeek - 1 - 1) % daysPerWeek) + 1

    val firstDayNumberInWeek: Int
  }


  val Day: DayCompanion


  object numberSystem extends TimeNumberSystem {
    protected override type Point = Moment

    protected val pointCreator: Creator[Moment] = Moment.apply
  }


  type TimeInterval = numberSystem.TimeInterval


  abstract class MomentBase(negative: Boolean, digits: List[Int])
    extends numberSystem.TimePoint(negative, digits) with CalendarMember[C]
  { this: Moment =>
    final def day: Day = Day(days + 1)

    final def time: TimeInterval = numberSystem.TimeInterval(negative = false, days(0).digits)
  }


  /**
   *
   */
  abstract class MomentCompanion {
    def apply(negative: Boolean, digits: List[Int]): Moment
  }


  val Moment: MomentCompanion

  final def moment: Moment = Moment(negative = false, List(0))  // This is def and not a val to make initialization possible

  final val interval: TimeInterval = numberSystem.TimeInterval(negative = false, List(0))

  final val week: TimeInterval = interval.days(7)
}
