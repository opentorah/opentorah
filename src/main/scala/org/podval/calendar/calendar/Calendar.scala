package org.podval.calendar.calendar

import org.podval.calendar.numbers.NumberSystem.RawNumber
import org.podval.calendar.time.{TimeInterval, TimeNumberSystem, TimePoint}
import org.podval.calendar.util.Numbered

trait Calendar[C <: Calendar[C]] extends TimeNumberSystem[C] { this: C =>

  type Year <: YearBase

  def createYear(number: Int): C#Year

  type YearCharacter

  type Month <: MonthBase[C]

  type MonthName

  def createMonth(number: Int): C#Month

  type Day <: DayBase[C]

  def createDay(number: Int): C#Day

  // TODO make this a Enum - and use its `values()` method in DayCompanion.name
  //   (which will then become `final`)?
  type DayName

  type Moment <: MomentBase[C]
  final override type Point = Moment

  def createMoment(raw: RawNumber): C#Moment
  final override def createPoint(raw: RawNumber): C#Point = Calendar.this.createMoment(raw)

  final override type Interval = TimeInterval[C]

  final override def createInterval(raw: RawNumber): Interval = new TimeInterval[C](raw) { this: C#Interval =>
    final override def numberSystem: C = Calendar.this
  }

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

    final def month(numberInYear: Int): C#Month = {
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
    final def apply(number: Int): C#Month = createMonth(number)

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


  abstract class MomentBase[T <: Calendar[T]](raw: RawNumber) extends TimePoint[T](raw) with CalendarMember[T]
  { this: T#Moment =>
    final def day: T#Day = calendar.createDay(days + 1)

    final def time: T#Interval = calendar.createInterval(false, days(0).digits)
  }


  val Moment: MomentCompanion[C]

  final val moment: C#Moment = createMoment(false, List(0))

  final val interval: C#Interval = createInterval(false, List(0))

  // TODO using Day.daysPerWeek will probably break initialization too...
  final val week: C#Interval = interval.days(7)
}
