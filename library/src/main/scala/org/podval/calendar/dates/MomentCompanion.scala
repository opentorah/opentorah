package org.podval.calendar.dates

import org.podval.calendar.numbers.NonPeriodicPointCompanion
import java.util.GregorianCalendar

abstract class MomentCompanion[C <: Calendar[C]] extends NonPeriodicPointCompanion[C] with CalendarMember[C] {
  def from(value: GregorianCalendar): C#Moment

  final def now: C#Moment = from(new GregorianCalendar)
}
