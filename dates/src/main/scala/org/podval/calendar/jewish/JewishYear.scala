package org.podval.calendar.jewish

import org.podval.calendar.dates.YearBase
import Jewish.{Day, Moment, Year, YearCharacter}

abstract class JewishYear(number: Int) extends YearBase[Jewish](number) {
  require(0 < number)

  final def newMoon: Moment = month(1).newMoon

  // TODO KH 7:8 says that postponement of RoshHashonoh is done to align the calendar better with
  // the true molad; analyze the statistics of distances between mean *and* true molad and RoshHashono.
  final override def firstDayNumber: Int = {
    val correction =
      if (isAduCorrected) 1
      else if (isFirstCorrected) 1 + (if (isFirstAduCorrected) 1 /* KH 7:3 */ else 0 /* KH 7:2 */)
      else if (isSecondCorrected) 2  /* KH 7:4 */
      else if (isThirdCorrected ) 1  /* KH 7:5 */
      else 0

    newMoon.day.number + correction
  }

  // KH 7:1
  final def isAduCorrected: Boolean = Year.isAdu(newMoon.day)

  // KH 7:2 (molad zoken)
  final def isFirstCorrected: Boolean = !isAduCorrected && (newMoon.time >= Year.firstCorrection)

  // KH 7:3
  final def isFirstAduCorrected: Boolean = isFirstCorrected && Year.isAdu(newMoon.day.next)

  // KH 7:4
  final def isSecondCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
    (newMoon.day.name == Day.Name.Shlishi) &&
    (newMoon.time >= Year.secondCorrection) &&
    !this.isLeap

  // KH 7:5
  // This is not defined for yer 0 - and doesn't apply :)
  final def isThirdCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
    !isSecondCorrected &&
    (newMoon.day.name == Day.Name.Sheni) &&
    (newMoon.time >= Year.thirdCorrection) &&
    this.prev.isLeap

  final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  final def cycle: Int = Year.cycle(number)

  final def numberInCycle: Int = Year.numberInCycle(number)

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
