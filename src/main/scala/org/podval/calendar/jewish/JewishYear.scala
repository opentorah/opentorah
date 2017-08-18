package org.podval.calendar.jewish

import org.podval.calendar.calendar.YearBase

abstract class JewishYear(number: Int) extends YearBase[Jewish](number) { this: Jewish#Year =>
  require(0 < number)

  final def newMoon: Jewish#Moment = month(1).newMoon

  final override def firstDayNumber: Int = {
    val correction =
      if (isAduCorrected) 1
      else if (isFirstCorrected) 1 + (if (isFirstAduCorrected) 1 /* KH 7:3 */ else 0 /* KH 7:2 */)
      else if (isSecondCorrected) 2  /* KH 7:4 */
      else if (isThirdCorrected ) 1  /* KH 7:5 */
      else 0

    newMoon.day.number + correction
  }

  final def isAduCorrected: Boolean = calendar.Year.isAdu(newMoon.day)  // KH 7:1

  final def isFirstCorrected: Boolean =
    !isAduCorrected && (newMoon.time >= calendar.Year.firstCorrection)

  final def isFirstAduCorrected: Boolean =
    isFirstCorrected && calendar.Year.isAdu(newMoon.day.next)

  final def isSecondCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
    (newMoon.day.name == calendar.Day.Name.Shlishi) &&
    (newMoon.time >= calendar.Year.secondCorrection) &&
    !this.isLeap

  // This is not defined for yer 0 - and doesn't apply :)
  final def isThirdCorrected: Boolean = !isAduCorrected && !isFirstCorrected &&
    !isSecondCorrected &&
    (newMoon.day.name == calendar.Day.Name.Sheni) &&
    (newMoon.time >= calendar.Year.thirdCorrection) &&
    this.prev.isLeap

  final override def lengthInDays: Int = next.firstDayNumber - this.firstDayNumber

  final def cycle: Int = calendar.Year.cycle(number)

  final def numberInCycle: Int = calendar.Year.numberInCycle(number)

  final override def character: Jewish#YearCharacter = (isLeap, kind)

  // KH 8:7,8
  final def kind: Jewish#YearKind = {
    val daysOverShort = lengthInDays - (if (isLeap) 383 else 353)

    daysOverShort match {
      case 0 => calendar.YearKind.Short
      case 1 => calendar.YearKind.Regular
      case 2 => calendar.YearKind.Full
      case _ => throw new IllegalArgumentException(
        "Impossible year length " + lengthInDays + " for " + this)
    }
  }
}
