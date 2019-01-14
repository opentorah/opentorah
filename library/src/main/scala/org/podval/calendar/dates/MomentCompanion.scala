package org.podval.calendar.dates

import org.podval.calendar.numbers.NumberCompanion
import java.util.GregorianCalendar

abstract class MomentCompanion[C <: Calendar[C]] extends NumberCompanion[C, C#Point] with CalendarMember[C] {
  def from(value: GregorianCalendar): C#Moment

  final def now: C#Moment = from(new GregorianCalendar)
}
