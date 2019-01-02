package org.podval.calendar.jewish

import org.podval.calendar.dates.DayBase

abstract class JewishDay(number: Int) extends DayBase[Jewish](number) {
  final def isShabbos: Boolean = is(Jewish.Day.Name.Shabbos)

  final def roshChodeshOf: Option[Jewish.Month.Name] =
    if (numberInMonth == 1) Some(month.name) else
    if (numberInMonth == 30) Some(month.next.name)
    else None

  final def isRoshChodesh: Boolean = roshChodeshOf.isDefined

  final def isShabbosMevarchim: Boolean = isShabbos && (shabbosAfter.month != this.month)

  final def shabbosAfter: Jewish.Day = next.next(Jewish.Day.Name.Shabbos)

  final def shabbosBefore: Jewish.Day = prev.prev(Jewish.Day.Name.Shabbos)
}
