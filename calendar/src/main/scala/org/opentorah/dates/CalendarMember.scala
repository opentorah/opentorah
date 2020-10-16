package org.opentorah.dates

import org.opentorah.numbers.NumbersMember

trait CalendarMember[C <: Calendar[C]] { this: NumbersMember[C] =>
  // TODO make calendar parameter a val where it is used and eliminate the body of this class - or even the class itself...
  final def calendar: C = numbers
}
