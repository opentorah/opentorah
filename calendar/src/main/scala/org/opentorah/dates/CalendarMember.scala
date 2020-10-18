package org.opentorah.dates

import org.opentorah.numbers.NumbersMember

trait CalendarMember[C <: Calendar[C]] extends NumbersMember[C] {
  final def calendar: C = numbers
}
