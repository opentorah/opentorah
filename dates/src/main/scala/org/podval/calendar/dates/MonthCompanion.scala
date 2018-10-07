package org.podval.calendar.dates

import org.podval.judaica.metadata.NamedCompanion

/**
  *
  */
abstract class MonthCompanion[C <: Calendar[C]] extends CalendarMember[C] {
  val Name: NamedCompanion

  // TODO push up into Calendar?
  final type Name = Name.Key

  def apply(number: Int): C#Month

  final def apply(year: Int, monthInYear: Int): C#Month =
    calendar.Year(year).month(monthInYear)

  def yearNumber(monthNumber: Int): Int

  def numberInYear(monthNumber: Int): Int
}
