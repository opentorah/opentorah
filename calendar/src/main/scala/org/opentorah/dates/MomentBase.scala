package org.opentorah.dates

import org.opentorah.metadata.{LanguageSpec, LanguageString}
import org.opentorah.numbers.Digits
import org.opentorah.times.TimePointBase

abstract class MomentBase[C <: Calendar[C]](digits: Digits)
  extends TimePointBase[C](digits) with LanguageString
{ this: C#Moment =>

  final def day: C#Day = numbers.Day(dayNumber)

  final def dayNumber: Int = days + 1

  final override def toLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + numbers.toString(time.hours) +
      ":" + numbers.toString(time.minutes) +
      "." + numbers.toString(time.partsWithoutMinutes) +
      "." + numbers.toString(time.moments)

  final def toSecondLanguageString(implicit spec: LanguageSpec): String =
    day.toLanguageString +
      " " + numbers.toString(time.hours) +
      ":" + numbers.toString(time.minutes) +
      ":" + numbers.toString(time.seconds) +
      "." + numbers.toString(time.milliseconds)
}
