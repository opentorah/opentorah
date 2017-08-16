package org.podval.calendar.dates

/**
  *
  * @param number  of the Day
  */
protected abstract class DayBase[C <: Calendar[C]](number: Int)
  extends Numbered[C#Day](number) with CalendarMember[C]
{ this: C#Day =>
  require(0 < number)

  final def next: C#Day = calendar.Day(number + 1)

  final def prev: C#Day = calendar.Day(number - 1)

  final def +(change: Int) = calendar.Day(number + change)

  final def -(change: Int) = calendar.Day(number - change)

  final def year: C#Year = calendar.Year(this)

  final def month: C#Month = year.monthForDay(numberInYear)

  final def numberInYear: Int = number - year.firstDayNumber + 1

  final def numberInMonth: Int = number - month.firstDayNumber + 1

  final def numberInWeek: Int = calendar.Day.numberInWeek(number)

  // TODO if DayBase is split into separate file (and parameterized with [C <: Calendar[C]]),
  // type of the name() method becomes `calendar.Day.Name`, causing compilation error:
  //   stable identifier required, but DayBase.this.calendar.Day found.
  // Conclusion: Day.Name type has to be moved into the Calendar itself first :(
  final def name: C#DayName = calendar.Day.names(numberInWeek - 1)

  final def toMoment: C#Moment = calendar.moment.days(number - 1)

  final override def toString: String = year + " " + month.name + " " + numberInMonth
}
