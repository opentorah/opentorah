package org.podval.calendar.dates

import Calendar.daysPerWeek

/**
  *
  */
abstract class DayCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  def names: Seq[C#DayName]

  final def apply(number: Int): C#Day =
    calendar.createDay(number)

  final def apply(year: Int, month: C#MonthName, day: Int): C#Day =
    calendar.Year(year).month(month).day(day)

  final def apply(year: Int, month: Int, day: Int): C#Day =
    calendar.Year(year).month(month).day(day)

  final def numberInWeek(dayNumber: Int): Int =
    ((dayNumber + firstDayNumberInWeek - 1 - 1) % daysPerWeek) + 1

  val firstDayNumberInWeek: Int
}
