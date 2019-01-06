package org.podval.calendar.dates

import org.podval.judaica.metadata.NamedCompanion

/**
  *
  */
abstract class MonthCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  val Name: NamedCompanion

  final type Name = Name.Key

  final def apply(year: C#Year, numberInYear: Int): C#Month =
    apply(year.firstMonthNumber + numberInYear - 1)

  def apply(number: Int): C#Month

  def yearNumber(monthNumber: Int): Int

  def numberInYear(monthNumber: Int): Int
}
