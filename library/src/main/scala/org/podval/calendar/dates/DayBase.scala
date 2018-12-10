package org.podval.calendar.dates

import org.podval.judaica.metadata.Numbered

/**
  *
  * @param number  of the Day
  */
abstract class DayBase[C <: Calendar[C]](number: Int)
  extends Numbered[C#Day](number) with CalendarMember[C]
{ this: C#Day =>
  require(0 < number)

  final def next: C#Day = this + 1

  final def prev: C#Day = this - 1

  final def +(change: Int): C#Day = calendar.Day(number + change)

  final def -(change: Int): C#Day = calendar.Day(number - change)

  final def -(that: C#Day): Int = this.number - that.number

  final def year: C#Year = calendar.Year(this)

  final def month: C#Month = year.monthForDay(numberInYear)

  final def numberInYear: Int = number - year.firstDayNumber + 1

  final def numberInMonth: Int = number - month.firstDayNumber + 1

  final def numberInWeek: Int = calendar.Day.numberInWeek(number)

  final def name: C#DayName = calendar.Day.names(numberInWeek - 1)

  final def is(name: C#DayName): Boolean = this.name == name

  final def next(dayName: C#DayName): C#Day = if (is(dayName)) this else this.next.next(dayName)

  final def prev(dayName: C#DayName): C#Day = if (is(dayName)) this else this.prev.prev(dayName)

  final def toMoment: C#Moment = calendar.Moment().days(number - 1)

  final override def toString: String = year + " " + month.name + " " + numberInMonth
}
