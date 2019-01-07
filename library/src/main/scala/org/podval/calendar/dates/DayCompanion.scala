package org.podval.calendar.dates

import org.podval.judaica.metadata.NamedCompanion

/**
  *
  */
abstract class DayCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  val Name: NamedCompanion

  final type Name = Name.Key

  def names: Seq[C#DayName]

  final def witNumberInMonth(month: C#Month, numberInMonth: Int): C#Day = {
    require (0 < numberInMonth && numberInMonth <= month.length)
    apply(month.firstDayNumber + numberInMonth - 1)
  }

  def apply(number: Int): C#Day

  final def numberInWeek(dayNumber: Int): Int =
    ((dayNumber + firstDayNumberInWeek - 1 - 1) % Calendar.daysPerWeek) + 1

  val firstDayNumberInWeek: Int

  final def now: C#Day = calendar.Moment.now.day
}
