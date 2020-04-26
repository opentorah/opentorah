package org.opentorah.calendar.dates

import org.opentorah.calendar.numbers.PointCompanion
import java.util.GregorianCalendar

abstract class MomentCompanion[C <: Calendar[C]] extends PointCompanion[C] with CalendarMember[C] {
  def from(value: GregorianCalendar): C#Moment

  final def now: C#Moment = from(new GregorianCalendar)
}
