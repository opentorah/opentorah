package org.podval.calendar.dates

import org.podval.calendar.numbers.NumbersMember

trait CalendarMember[C <: Calendar[C]] extends NumbersMember[C] {
  final def calendar: C = numbers
}
