package org.podval.calendar.calendar

trait CalendarMember[C <: Calendar[C]] {
  def calendar: C
}
