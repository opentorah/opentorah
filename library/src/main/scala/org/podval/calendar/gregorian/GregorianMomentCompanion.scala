package org.podval.calendar.gregorian

import org.podval.calendar.dates.MomentCompanion
import java.util.{GregorianCalendar, Calendar => Cal}

abstract class GregorianMomentCompanion extends MomentCompanion[Gregorian] {
  final override def from(value: GregorianCalendar): Gregorian#Moment = Gregorian
    .Year(value.get(Cal.YEAR))
    .month(value.get(Cal.MONTH+1))
    .day(value.get(Cal.DAY_OF_MONTH)).toMoment
    .hours(value.get(Cal.HOUR_OF_DAY))
    .minutes(value.get(Cal.MINUTE))
    .secondsAndMilliseconds(value.get(Cal.SECOND), value.get(Cal.MILLISECOND))
}
