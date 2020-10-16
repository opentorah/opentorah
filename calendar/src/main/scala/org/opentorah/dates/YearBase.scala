package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString, Numbered}
import org.opentorah.numbers.NumbersMember

/**
  *
  * @param yearNumber  number of the Year
  */
abstract class YearBase[C <: Calendar[C]](calendar: C, yearNumber: Int)
  extends NumbersMember[C](calendar) with CalendarMember[C] with Numbered[C#Year] with LanguageString
{ this: C#Year =>
  override def number: Int = yearNumber

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

  final def month(numberInYear: Int): C#Month =
    calendar.Month.withNumberInYear(this, numberInYear)

  final def containsMonth(name: C#MonthName): Boolean =
    monthDescriptors.exists(_.name == name)

  final def month(name: C#MonthName): C#Month =
    month(monthDescriptors.indexWhere(_.name == name) + 1)

  final def monthAndDay(when: MonthAndDay[C]): C#Day =
    month(when.monthName).day(when.numberInMonth)

  private[opentorah] final def monthForDay(day: Int): C#Month = {
    require(0 < day && day <= lengthInDays)
    month(monthDescriptors.count(_.daysBefore < day))
  }

  private[opentorah] final def monthDescriptors: Seq[C#MonthDescriptor] =
    calendar.Year.monthDescriptors(character)

  final override def toLanguageString(implicit spec: LanguageSpec): String = calendar.toString(number)
}
