package org.podval.calendar.dates

trait CalendarMember[C <: Calendar[C]] {
  def calendar: C
}
