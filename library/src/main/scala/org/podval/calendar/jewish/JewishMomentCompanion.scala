package org.podval.calendar.jewish

import org.podval.calendar.dates.{Calendar, MomentCompanion}
import org.podval.calendar.gregorian.Gregorian
import java.util.GregorianCalendar

abstract class JewishMomentCompanion extends MomentCompanion[Jewish] {
  final override def from(value: GregorianCalendar): Jewish#Moment =
    Calendar.toJewish(Gregorian.Moment.from(value))
}
