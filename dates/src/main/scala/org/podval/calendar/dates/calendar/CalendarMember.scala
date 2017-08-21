package org.podval.calendar.dates.calendar

trait CalendarMember[C <: Calendar[C]] {
  def calendar: C
}
