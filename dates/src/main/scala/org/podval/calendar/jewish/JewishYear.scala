package org.podval.calendar.jewish

import org.podval.calendar.dates.YearBase
import Jewish.{Year, Moment, YearCharacter}

abstract class JewishYear(number: Int) extends YearBase[Jewish](number) {
  require(0 < number)

  final def newMoon: Moment = month(1).newMoon

  final def newYearDelay: NewYear.Delay = NewYear.delay(number, newMoon)

  final override def firstDayNumber: Int = newMoon.day.number + newYearDelay.days

  final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  final override def character: YearCharacter = (isLeap, kind)

  // KH 8:7-8
  final def kind: Year.Kind = {
    val daysOverShort: Int = lengthInDays - (if (isLeap) 383 else 353)

    daysOverShort match {
      case 0 => Year.Kind.Short
      case 1 => Year.Kind.Regular
      case 2 => Year.Kind.Full
      case _ => throw new IllegalArgumentException(
        "Impossible year length " + lengthInDays + " for " + this)
    }
  }
}
