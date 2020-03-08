package org.podval.calendar.dates

import org.opentorah.metadata.{LanguageSpec, Numbered}

/**
  *
  * @param number  of the Month
  */
abstract class MonthBase[C <: Calendar[C]] private[calendar](private var yearOpt: Option[C#Year], number: Int)
  extends Numbered[C#Month](number) with CalendarMember[C]
{ this: C#Month =>
  require(0 < number)

  final def year: C#Year = {
    if (yearOpt.isEmpty) {
      yearOpt = Some(calendar.Year(calendar.Month.yearNumber(number)))
    }

    yearOpt.get
  }

  final def next: C#Month = this + 1

  final def prev: C#Month = this - 1

  final def +(change: Int): C#Month = calendar.Month(number + change)

  final def -(change: Int): C#Month = calendar.Month(number - change)

  final def numberInYear: Int = calendar.Month.numberInYear(number)

  final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

  final def firstDay: C#Day = day(1)

  final def lastDay: C#Day = day(length)

  final def days: Seq[C#Day] = (1 to length).map(day)

  final def day(numberInMonth: Int): C#Day = calendar.Day.witNumberInMonth(this, numberInMonth)

  final def name: C#MonthName = descriptor.name

  final def length: Int = descriptor.length

  private[this] def descriptor: C#MonthDescriptor = year.monthDescriptors(numberInYear - 1)

  final def numberInYearToLanguageString(implicit spec: LanguageSpec): String = calendar.toString(numberInYear)
}
