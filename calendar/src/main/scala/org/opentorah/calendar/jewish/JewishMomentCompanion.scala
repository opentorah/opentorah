package org.opentorah.calendar.jewish

import org.opentorah.calendar.dates.{Calendar, MomentCompanion}
import org.opentorah.calendar.gregorian.Gregorian
import java.util.GregorianCalendar

abstract class JewishMomentCompanion extends MomentCompanion[Jewish] {
  final override def from(value: GregorianCalendar): Jewish#Moment =
    Calendar.toJewish(Gregorian.Moment.from(value))
}
