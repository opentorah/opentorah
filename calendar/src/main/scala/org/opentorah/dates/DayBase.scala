package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString, Numbered}
import org.opentorah.numbers.NumbersMember

trait DayBase[C <: Calendar[C]] extends NumbersMember[C] with Numbered with LanguageString { this: C#Day =>

  type T = C#Day

  protected var monthOpt: Option[C#Month]

  require(0 < number)

  final def month: C#Month = {
    if (monthOpt.isEmpty) {
      var year: C#Year = numbers.Year(numbers.Year.yearsForSureBefore(number))
      require(year.firstDayNumber <= number)
      while (year.next.firstDayNumber <= number) year = year.next

      monthOpt = Some(year.monthForDay(number - year.firstDayNumber + 1))
    }

    monthOpt.get
  }

  final def next: C#Day = this + 1

  final def prev: C#Day = this - 1

  final def +(change: Int): C#Day = numbers.Day(number + change)

  final def -(change: Int): C#Day = numbers.Day(number - change)

  final def -(that: C#Day): Int = this.number - that.number

  final def year: C#Year = month.year

  final def numberInYear: Int = number - year.firstDayNumber + 1

  final def numberInMonth: Int = number - month.firstDayNumber + 1

  final def numberInWeek: Int = numbers.Day.numberInWeek(number)

  final def name: C#DayName = numbers.Day.names(numberInWeek - 1)

  final def is(name: C#DayName): Boolean = this.name == name

  // TODO get rid of the cast!
  final def monthAndDay: numbers.MonthAndDay = new numbers.MonthAndDay(month.name.asInstanceOf[numbers.Month.Name], numberInMonth)

  @scala.annotation.tailrec
  final def next(dayName: C#DayName): C#Day = if (is(dayName)) this else this.next.next(dayName)

  @scala.annotation.tailrec
  final def prev(dayName: C#DayName): C#Day = if (is(dayName)) this else this.prev.prev(dayName)

  final def toMoment: C#Moment = numbers.Moment().days(number - 1)

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    year.toLanguageString + " " + month.name.toLanguageString + " " + numberInMonthToLanguageString

  final def numberInMonthToLanguageString(implicit spec: LanguageSpec): String = numbers.toString(numberInMonth)
}
