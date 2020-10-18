package org.opentorah.calendar.gregorian

import org.opentorah.dates.{CalendarMember, MomentCompanion}

trait GregorianMomentCompanion extends MomentCompanion[Gregorian] with CalendarMember[Gregorian] {
  this: MomentCompanion[Gregorian] =>
}