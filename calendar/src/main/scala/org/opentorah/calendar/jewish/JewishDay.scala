package org.opentorah.calendar.jewish

import org.opentorah.dates.DayBase

abstract class JewishDay private[jewish](monthOpt: Option[Jewish.Month], number: Int)
  extends DayBase[Jewish](monthOpt, number)
{
  def isShabbos: Boolean = is(Jewish.Day.Name.Shabbos)

  def roshChodeshOf: Option[Jewish.Month.Name] =
    if (numberInMonth == 1) Some(month.name) else
    if (numberInMonth == 30) Some(month.next.name)
    else None

  def isRoshChodesh: Boolean = roshChodeshOf.isDefined

  def isShabbosMevarchim: Boolean = isShabbos && (shabbosAfter.month != this.month)

  def shabbosAfter: Jewish.Day = next.next(Jewish.Day.Name.Shabbos)

  def shabbosBefore: Jewish.Day = prev.prev(Jewish.Day.Name.Shabbos)
}
