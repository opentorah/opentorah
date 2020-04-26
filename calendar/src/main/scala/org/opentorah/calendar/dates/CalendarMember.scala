package org.opentorah.calendar.dates

import org.opentorah.calendar.numbers.NumbersMember

trait CalendarMember[C <: Calendar[C]] extends NumbersMember[C] {
  final def calendar: C = numbers
}
