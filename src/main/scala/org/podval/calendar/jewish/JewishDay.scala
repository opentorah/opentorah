package org.podval.calendar.jewish

import org.podval.calendar.calendar.DayBase

abstract class JewishDay(number: Int) extends DayBase[Jewish](number) {
  this: Jewish.Day =>
}
