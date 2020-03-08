package org.podval.calendar.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString, Numbered}

/**
  *
  * @param number  of the Day
  */
abstract class DayBase[C <: Calendar[C]] private[calendar](private var monthOpt: Option[C#Month], number: Int)
  extends Numbered[C#Day](number) with CalendarMember[C] with LanguageString
{ this: C#Day =>
  require(0 < number)

  final def month: C#Month = {
    if (monthOpt.isEmpty) {
      var year: C#Year = calendar.Year(calendar.Year.yearsForSureBefore(number))
      require(year.firstDayNumber <= number)
      while (year.next.firstDayNumber <= number) year = year.next

      monthOpt = Some(year.monthForDay(number - year.firstDayNumber + 1))
    }

    monthOpt.get
  }

  final def next: C#Day = this + 1

  final def prev: C#Day = this - 1

  final def +(change: Int): C#Day = calendar.Day(number + change)

  final def -(change: Int): C#Day = calendar.Day(number - change)

  final def -(that: C#Day): Int = this.number - that.number

  final def year: C#Year = month.year

  final def numberInYear: Int = number - year.firstDayNumber + 1

  final def numberInMonth: Int = number - month.firstDayNumber + 1

  final def numberInWeek: Int = calendar.Day.numberInWeek(number)

  final def name: C#DayName = calendar.Day.names(numberInWeek - 1)

  final def is(name: C#DayName): Boolean = this.name == name

  final def monthAndDay: MonthAndDay[C] = new MonthAndDay[C](month.name, numberInMonth)

  final def next(dayName: C#DayName): C#Day = if (is(dayName)) this else this.next.next(dayName)

  final def prev(dayName: C#DayName): C#Day = if (is(dayName)) this else this.prev.prev(dayName)

  final def toMoment: C#Moment = calendar.Moment().days(number - 1)

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    year.toLanguageString + " " + month.name.toLanguageString + " " + numberInMonthToLanguageString

  final def numberInMonthToLanguageString(implicit spec: LanguageSpec): String = calendar.toString(numberInMonth)
}
