package org.podval.calendar.dates

import Calendar.daysPerWeek
import org.podval.judaica.metadata.NamedCompanion

/**
  *
  */
abstract class DayCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  val Name: NamedCompanion

  // TODO push up into Calendar?
  final type Name = Name.Key

  def names: Seq[C#DayName]

  def apply(number: Int): C#Day

  final def apply(year: Int, month: C#MonthName, day: Int): C#Day =
    calendar.Year(year).month(month).day(day)

  final def apply(year: Int, month: Int, day: Int): C#Day =
    calendar.Year(year).month(month).day(day)

  final def numberInWeek(dayNumber: Int): Int =
    ((dayNumber + firstDayNumberInWeek - 1 - 1) % daysPerWeek) + 1

  val firstDayNumberInWeek: Int
}
