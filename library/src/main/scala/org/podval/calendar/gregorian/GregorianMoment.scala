package org.podval.calendar.gregorian

import org.podval.calendar.dates.MomentBase
import org.podval.judaica.metadata.LanguageSpec
import Gregorian.Moment

trait GregorianMoment extends MomentBase[Gregorian] {
  final def morningHours(value: Int): Moment = firstHalfHours(value)

  final def afternoonHours(value: Int): Moment = secondHalfHours(value)

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      ":" + calendar.toString(time.seconds) +
      "." + calendar.toString(time.milliseconds)
}
