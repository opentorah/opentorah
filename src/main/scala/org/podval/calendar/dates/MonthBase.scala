package org.podval.calendar.dates

import org.podval.calendar.util.Numbered

/**
  *
  * @param number  of the Month
  */
abstract class MonthBase[C <: Calendar[C]](number: Int)
  extends Numbered[C#Month](number) with CalendarMember[C]
{ this: C#Month =>
  require(0 < number)

  final def next: C#Month = calendar.Month(number + 1)

  final def prev: C#Month = calendar.Month(number - 1)

  final def +(change: Int) = calendar.Month(number + change)

  final def -(change: Int) = calendar.Month(number - change)

  final def year: C#Year = calendar.Year(this)

  final def numberInYear: Int = calendar.Month.numberInYear(number)

  final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

  final def firstDay: C#Day = day(1)

  final def lastDay: C#Day = day(length)

  final def days: Seq[C#Day] = (1 to length).map(day)

  final def day(numberInMonth: Int): C#Day = {
    require (0 < numberInMonth && numberInMonth <= length)
    calendar.Day(firstDayNumber + numberInMonth - 1)
  }

  final def name: C#MonthName = descriptor.name

  final def length: Int = descriptor.length

  private[this] def descriptor = year.monthDescriptors(numberInYear - 1)
}
