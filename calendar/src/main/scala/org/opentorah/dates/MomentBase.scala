package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString}
import org.opentorah.numbers.Digits
import org.opentorah.times.TimePointBase

abstract class MomentBase[C <: Calendar[C]](digits: Digits)
  extends TimePointBase[C](digits) with CalendarMember[C] with LanguageString
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
