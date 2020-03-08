package org.podval.calendar.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString}
import org.podval.calendar.times.TimePointBase

trait MomentBase[C <: Calendar[C]] extends TimePointBase[C] with CalendarMember[C] with LanguageString
{ this: C#Moment =>

  final def day: C#Day = calendar.Day(dayNumber)

  final def dayNumber: Int = days + 1

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      "." + calendar.toString(time.partsWithoutMinutes) +
      "." + calendar.toString(time.moments)

  final def toSecondLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + calendar.toString(time.hours) +
      ":" + calendar.toString(time.minutes) +
      ":" + calendar.toString(time.seconds) +
      "." + calendar.toString(time.milliseconds)
}
