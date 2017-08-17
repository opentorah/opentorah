package org.podval.calendar.calendar

import org.podval.calendar.util.Numbered

trait Calendar[C <: Calendar[C]] { this: C =>

  type Year <: YearBase

  def createYear(number: Int): Year

  type Month <: MonthBase[C]

  def createMonth(number: Int): Month

  type Day <: DayBase[C]

  def createDay(number: Int): Day

  type Moment <: MomentBase

  def createMoment(negative: Boolean, digits: List[Int]): Moment


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

    final def firstDay: C#Day = firstMonth.firstDay

    final def lastDay: C#Day = lastMonth.lastDay

    def firstDayNumber: Int

    def lengthInDays: Int

    final def days: Seq[C#Day] = months.flatMap(_.days)

    final def firstMonth: Month = month(1)

    final def lastMonth: Month = month(lengthInMonths)

    final def firstMonthNumber: Int = Year.firstMonth(number)

    final def lengthInMonths: Int = Year.lengthInMonths(number)

    final def months: Seq[Month] = (1 to lengthInMonths).map(month)

    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      Month(firstMonthNumber + numberInYear - 1)
    }

    final def month(name: C#MonthName): Month = month(monthDescriptors.indexWhere(_.name == name) + 1)

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
  abstract class YearCompanion extends CalendarMember[C] {
    final def apply(number: Int): Year = createYear(number)

    final def apply(month: C#Month): Year = apply(Month.yearNumber(month.number))

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

  val Year: YearCompanion


  type MonthName


  /**
   *
   */
  abstract class MonthCompanion extends CalendarMember[C] {
    final def apply(number: Int): Month = createMonth(number)

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

  val Day: DayCompanion[C]


  object numberSystem extends TimeNumberSystem {
    protected override type Point = Moment

    protected val pointCreator: Creator[Moment] = Moment.apply
  }


  type TimeInterval = numberSystem.TimeInterval


  abstract class MomentBase(negative: Boolean, digits: List[Int])
    extends numberSystem.TimePoint(negative, digits) with CalendarMember[C]
  { this: Moment =>
    final def day: C#Day = Day(days + 1)

    final def time: TimeInterval = numberSystem.TimeInterval(negative = false, days(0).digits)
  }


  /**
   *
   */
  abstract class MomentCompanion extends CalendarMember[C] {
    final def apply(negative: Boolean, digits: List[Int]): Moment = createMoment(negative, digits)
  }


  val Moment: MomentCompanion

  final def moment: Moment = Moment(negative = false, List(0))  // This is def and not a val to make initialization possible

  final val interval: TimeInterval = numberSystem.TimeInterval(negative = false, List(0))

  final val week: TimeInterval = interval.days(7)
}
