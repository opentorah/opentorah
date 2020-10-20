package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, Numbered}
import org.opentorah.numbers.NumbersMember

trait MonthBase[C <: Calendar[C]] extends NumbersMember[C] with Numbered { this: C#Month =>

  type T = C#Month

  protected var yearOpt: Option[C#Year]

  require(0 < number)

  final def year: C#Year = {
    if (yearOpt.isEmpty) {
      yearOpt = Some(numbers.Year(numbers.Month.yearNumber(number)))
    }

    yearOpt.get
  }

  final def next: C#Month = this + 1

  final def prev: C#Month = this - 1

  final def +(change: Int): C#Month = numbers.Month(number + change)

  final def -(change: Int): C#Month = numbers.Month(number - change)

  final def numberInYear: Int = numbers.Month.numberInYear(number)

  final def firstDayNumber: Int = year.firstDayNumber + descriptor.daysBefore

  final def firstDay: C#Day = day(1)

  final def lastDay: C#Day = day(length)

  final def days: Seq[C#Day] = (1 to length).map(day)

  final def day(numberInMonth: Int): C#Day = numbers.Day.witNumberInMonth(this, numberInMonth)

  final def name: C#MonthName = descriptor.name

  final def length: Int = descriptor.length

  private[this] def descriptor: C#MonthDescriptor = year.monthDescriptors(numberInYear - 1)

  final def numberInYearToLanguageString(implicit spec: LanguageSpec): String = numbers.toString(numberInYear)
}
