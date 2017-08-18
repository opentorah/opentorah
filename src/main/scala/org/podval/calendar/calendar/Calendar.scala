package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.util.Numbered

trait Calendar[C <: Calendar[C]] { this: C =>

  type Year <: YearBase

  def createYear(number: Int): C#Year

  type YearCharacter

  type Month <: MonthBase[C]

  type MonthName

  def createMonth(number: Int): Month // TODO prefix

  type Day <: DayBase[C]

  def createDay(number: Int): C#Day

  // TODO make this a Enum - and use its `values()` method in DayCompanion.name
  //   (which will then become `final`)?
  type DayName

  // When Calendar inherits from TimeNumberSystem, I get an error in Sun.scala:
  //   overloaded method value - with alternatives:
  //   ((that: _1.JewishMoment)_1.TimeInterval) forSome { val _1: org.podval.calendar.jewish.Jewish } <and>
  //   ((that: _1.TimeInterval)_1.JewishMoment) forSome { val _1: org.podval.calendar.jewish.Jewish }
  //   cannot be applied to (org.podval.calendar.jewish.Jewish.Interval)
  //   val firstTkufasNissan = Year(1).month(MonthName.Nisan).newMoon - interval.days(7).hours(9).parts(642)  // KH 9:3
  //
  // Even if I replace abstract types Moment with Point and TimeInterval with Interval,
  // the error persists. I may still have to unify the abstract types, since `type A = B`
  // doesn't help, but there is a different reason for the error...

  type Moment <: MomentBase

  def createMoment(raw: RawNumber): Moment // TODO prefix

  object numberSystem extends TimeNumberSystem {
    final override type Point = Moment
    final override type Interval = TimeInterval

    final override def createInterval(raw: RawNumber): Interval = new TimeInterval(raw)

    final override def createPoint(raw: RawNumber): Point = Calendar.this.createMoment(raw)
  }

  type TimeInterval = numberSystem.TimeInterval

  // TODO prefix
  final def createInterval(raw: RawNumber): TimeInterval = numberSystem.createInterval(raw)

  /**
   *
   * @param number  of the Year
   */
  abstract class YearBase(number: Int)
    extends Numbered[C#Year](number) with CalendarMember[C]
  { this: C#Year =>
    def character: C#YearCharacter

    final def isLeap: Boolean = calendar.Year.isLeap(number)

    final def next: C#Year = this + 1

    final def prev: C#Year = this - 1

    final def +(change: Int): C#Year = calendar.Year(number + change)

    final def -(change: Int): C#Year = calendar.Year(number - change)

    final def firstDay: C#Day = firstMonth.firstDay

    final def lastDay: C#Day = lastMonth.lastDay

    def firstDayNumber: Int

    def lengthInDays: Int

    final def days: Seq[C#Day] = months.flatMap(_.days)

    final def firstMonth: C#Month = month(1)

    final def lastMonth: C#Month = month(lengthInMonths)

    final def firstMonthNumber: Int = calendar.Year.firstMonth(number)

    final def lengthInMonths: Int = calendar.Year.lengthInMonths(number)

    final def months: Seq[C#Month] = (1 to lengthInMonths).map(month)

    // TODO prefix
    final def month(numberInYear: Int): Month = {
      require(0 < numberInYear && numberInYear <= lengthInMonths)
      Month(firstMonthNumber + numberInYear - 1)
    }

    final def month(name: C#MonthName): C#Month =
      month(monthDescriptors.indexWhere(_.name == name) + 1)

    final def monthForDay(day: Int): C#Month = {
      require(0 < day && day <= lengthInDays)
      month(monthDescriptors.count(_.daysBefore < day))
    }

    final def monthDescriptors: List[C#MonthDescriptor] =
      calendar.Year.monthDescriptors(character)
  }


  val Year: YearCompanion[C]


  /**
   *
   */
  abstract class MonthCompanion extends CalendarMember[C] {
    final def apply(number: Int): Month = createMonth(number) // TODO prefix

    final def apply(year: Int, monthInYear: Int): C#Month =
      calendar.createYear(year).month(monthInYear)

    def yearNumber(monthNumber: Int): Int

    def numberInYear(monthNumber: Int): Int
  }


  final class MonthNameAndLength(val name: C#MonthName, val length: Int)


  final def createMonthNameAndLength(name: C#MonthName, length: Int):
  C#MonthNameAndLength =
    new MonthNameAndLength(name, length)


  final class MonthDescriptor(val name: C#MonthName, val length: Int, val daysBefore: Int)


  final def createMonthDescriptor(name: C#MonthName, length: Int, daysBefore: Int):
  C#MonthDescriptor =
    new MonthDescriptor(name, length, daysBefore)

  val Month: MonthCompanion

  val Day: DayCompanion[C]


  abstract class MomentBase(raw: RawNumber)
    extends numberSystem.TimePoint(raw) with CalendarMember[C]
  { this: Moment => // TODO prefix
    final def day: C#Day = Day(days + 1)

    final def time: TimeInterval = createInterval(false, days(0).digits) // TODO prefix
  }


  val Moment: MomentCompanion[C]

  final val moment: Moment = createMoment(false, List(0)) // TODO prefix

  final val interval: TimeInterval = createInterval(false, List(0)) // TODO prefix

  // TODO using Day.daysPerWeek will probably break initialization too...
  final val week: TimeInterval = interval.days(7) // TODO prefix
}
