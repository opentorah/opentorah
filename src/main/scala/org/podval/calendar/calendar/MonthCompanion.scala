package org.podval.calendar.calendar

/**
  *
  */
abstract class MonthCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  final def apply(number: Int): C#Month = calendar.createMonth(number)

  final def apply(year: Int, monthInYear: Int): C#Month =
    calendar.createYear(year).month(monthInYear)

  def yearNumber(monthNumber: Int): Int

  def numberInYear(monthNumber: Int): Int
}
