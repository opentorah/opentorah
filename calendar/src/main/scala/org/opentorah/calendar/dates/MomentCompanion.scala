package org.opentorah.calendar.dates

import java.util.GregorianCalendar
import org.opentorah.numbers.PointCompanion

abstract class MomentCompanion[C <: Calendar[C]] extends PointCompanion[C] with CalendarMember[C] {
  def from(value: GregorianCalendar): C#Moment

  final def now: C#Moment = from(new GregorianCalendar)
}
