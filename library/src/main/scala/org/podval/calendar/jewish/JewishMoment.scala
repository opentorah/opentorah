package org.podval.calendar.jewish

import org.podval.calendar.dates.MomentBase
import org.podval.judaica.metadata.LanguageSpec
import Jewish.Moment

trait JewishMoment extends MomentBase[Jewish] {
  final def nightHours(value: Int): Moment = firstHalfHours(value)

  final def dayHours(value: Int): Moment = secondHalfHours(value)

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      "." + calendar.toString(time.partsWithoutMinutes) +
      "." + calendar.toString(time.moments)
}
