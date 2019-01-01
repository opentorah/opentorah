package org.podval.calendar.dates

import org.podval.calendar.times.TimePointBase
import org.podval.judaica.metadata.{LanguageSpec, LanguageString}

trait MomentBase[C <: Calendar[C]] extends TimePointBase[C] with CalendarMember[C] with LanguageString
{ this: C#Moment =>

  final def day: C#Day = calendar.Day(days + 1)

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      "." + calendar.toString(time.partsWithoutMinutes) +
      "." + calendar.toString(time.moments)

  final def toGregorianLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      ":" + calendar.toString(time.seconds) +
      "." + calendar.toString(time.milliseconds)
}
