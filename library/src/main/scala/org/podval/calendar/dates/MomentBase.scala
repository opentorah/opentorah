package org.podval.calendar.dates

import org.podval.calendar.times.TimePointBase
import org.podval.judaica.metadata.LanguageString

trait MomentBase[C <: Calendar[C]] extends TimePointBase[C] with CalendarMember[C] with LanguageString
{ this: C#Moment =>

  final def day: C#Day = calendar.Day(dayNumber)

  final def dayNumber: Int = days + 1
}
